open! Core
open! Async
open! Import

module Command = Core.Command

module Mode = struct
  module T = struct
    type t = [ `Prod | `Dev ] [@@deriving compare]

    let to_string = function
      | `Prod -> "prod"
      | `Dev  -> "dev"
    ;;

    let of_string = function
      | "Prod" | "prod" -> `Prod
      | "Dev"  | "dev"  -> `Dev
      | str -> failwithf "Illegal string %s. Expected 'prod' or 'dev'." str ()
    ;;
  end
  include T
  include Sexpable.Of_stringable (T)
end

let common_flags ~appdir_for_doc ~appdir =
  Command.Spec.(
    step (fun k ~appdir ~mode ->
      let basedir = appdir ^/ Mode.to_string mode in
      k ~basedir ~mode
    )
    ++ step (fun k x -> k ~appdir:x)
    +> flag "-appdir" (optional_with_default appdir file)
         ~doc:("DIR override default APPDIR of " ^ appdir_for_doc)
    ++ step (fun k x -> k ~mode:x)
    +> flag "-mode" (optional_with_default `Prod (Arg_type.create Mode.of_string))
         ~doc:"MODE running mode, prod/dev (default = prod)"
  )
;;

module Lock_file = struct
  let path ~lockdir = lockdir ^/ "lock"

  let create_exn ~lockdir =
    let timeout = Core.Time.Span.of_sec 1. in
    let lock_file = path ~lockdir in
    Core.Lock_file.Nfs.blocking_create ~timeout lock_file
  ;;

  let read_exn ~basedir:lockdir =
    let lock_file = path ~lockdir in
    let is_locked =
      try Core.Lock_file.Nfs.critical_section
            ~timeout:Core.Time.Span.zero lock_file ~f:(fun () -> false)
      with _ -> true
    in
    if not is_locked
    then failwithf "Lock file %s is not locked by any process" lock_file ()
    else (
      match Core.Lock_file.Nfs.get_hostname_and_pid lock_file with
      | None -> failwithf "unable to read hostname and pid from %s" lock_file ()
      | Some (host, pid) ->
        let my_host = Unix.gethostname () in
        if String.(<>) host my_host
        then
          failwithf "Hostname in lockfile %s doesn't match current hostname %s"
            host my_host ()
        else (host, pid))
  ;;
end

let configure_log ~log_format ~logdir ~fg =
  let log_rotation = Log.Rotation.default () in
  Log.Global.set_level `Info;
  let output =
    Log.Output.rotating_file log_format log_rotation
      ~basename:(logdir ^/ "messages")
  in
  let outputs = [output] in
  Log.Global.set_output (if fg then Log.Output.stderr () :: outputs else outputs)
;;

let start ~init_stds ~log_format ~main ~basedir ~mode ~fg () =
  let keep_stdout_and_stderr = not fg in
  let logdir = basedir in
  let release_io =
    if fg then
      (fun () -> ())
    else (
      let redir filename =
        if keep_stdout_and_stderr
        then Some (`File_append (logdir ^/ filename))
        else None
      in
      Daemon.daemonize_wait
        ?redirect_stdout:(redir "stdout") ?redirect_stderr:(redir "stderr")
        ~cd:basedir ()
      |> unstage
    )
  in
  Lock_file.create_exn ~lockdir:basedir;
  configure_log ~log_format ~logdir ~fg;
  Signal.handle [Signal.term; Signal.int] ~f:(fun signal ->
    if keep_stdout_and_stderr
    then Core.Printf.printf !"shutting down upon receiving signal %{Signal} at %{Time}\n%!"
           signal (Time.now ());
    Log.Global.info !"shutting down upon receiving signal %{Signal}" signal;
    upon (Log.Global.flushed ()) (fun () ->
      shutdown 0
    )
  );
  let tags =
    [ "pid", Pid.to_string (Unix.getpid ())
    ; "version", Version_util.version
    ; "build info", Version_util.build_info
    ; "command line", String.concat ~sep:" " (Array.to_list Sys.argv)
    ]
  in
  upon Deferred.unit (fun () ->
    release_io ();
    if keep_stdout_and_stderr && init_stds
    then (
      (* Multiple runs usually append to the same "keep" files, so these separator
         lines are helpful for distinguishing the output of one run from another. *)
      let now = Core.Time.now () in
      List.iter [ Pervasives.stdout ; Pervasives.stderr ] ~f:(fun oc ->
        Core.Printf.fprintf oc !"%s Daemonized with tags=%{Sexp}\n%!"
          (Core.Time.to_string_abs now ~zone:Core.(force Time.Zone.local))
          ([%sexp_of: (string * string) list] tags)
      ))
  );
  let main =
    Monitor.try_with ~extract_exn:true (fun () ->
      Log.Global.info ~tags !"Starting up";
      (match mode with
       | `Prod -> ()
       | `Dev ->
         Log.Global.set_level `Debug;
         Log.Global.debug "logging at level `Debug because we're in dev mode";
      );
      main ~basedir
    )
  in
  upon main (function
    | Ok () -> shutdown 0
    | Error e ->
      let e = Error.tag (Error.of_exn e) ~tag:"app_harness: error escaped main" in
      Log.Global.error "%s" (Error.to_string_hum e);
      shutdown 1
  );
  (never_returns (Scheduler.go ()) : unit)
;;

let start_command ~appname ~appdir_for_doc ~appdir ~log_format spec main =
  let readme () = sprintf "BASEDIR is by default %s/MODE" appdir_for_doc in
  Command.basic ~summary:("start " ^ appname)
    ~readme
    Command.Spec.(
      spec
      ++ step (fun main -> start ~init_stds:true ~log_format ~main)
      ++ common_flags ~appdir_for_doc ~appdir
      ++ step (fun k x -> k ~fg:x)
      +> flag "-fg" no_arg ~doc:" run in foreground, don't daemonize"
    )
    main
;;

let stop_command ~appname ~appdir_for_doc ~appdir =
  Command.basic ~summary:("stop " ^ appname)
    (common_flags ~appdir_for_doc ~appdir)
    (fun ~basedir ~mode:_ () ->
       let _, pid = Lock_file.read_exn ~basedir in
       match Signal.send Signal.term (`Pid pid) with
       | `Ok -> ()
       | `No_such_process -> failwithf !"Attempt to kill nonexistent pid %{Pid}" pid ()
    )
;;

let status_command ~appname ~appdir_for_doc ~appdir =
  Command.basic ~summary:("status of " ^ appname)
    (common_flags ~appdir_for_doc ~appdir)
    (fun ~basedir ~mode:_ () ->
       try
         let host, pid = Lock_file.read_exn ~basedir in
         Core.printf "%s: RUNNING on host %s pid %d\n%!" appname host (Pid.to_int pid);
         Pervasives.exit 0
       with exn ->
         Core.printf "%s: NOT RUNNING %s\n%!" appname (Exn.to_string exn);
         Pervasives.exit 1
    )
;;

let commands ~appname ~appdir_for_doc ~appdir ~log_format ~start_spec ~start_main =
  [ "start"  , start_command  ~appname ~appdir_for_doc ~appdir ~log_format
                 start_spec start_main
  ; "stop"   , stop_command   ~appname ~appdir_for_doc ~appdir
  ; "status" , status_command ~appname ~appdir_for_doc ~appdir
  ]
;;
