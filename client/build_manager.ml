open Core
open Async
open Import

let send_sigkill_if_pid_exists pid =
  let signal = Signal.kill in
  match%map
    In_thread.syscall ~name:"send_signal" (fun () ->
      Signal.send_i signal (`Pid pid))
    |> Clock.with_timeout (sec 0.2)
  with
  | `Result (Ok ()) -> Ok ()
  | `Result (Error exn) ->
    error_s
      [%sexp
        "sending signal failed",
        { pid    : Pid.t
        ; signal : Signal.t
        ; exn    : Exn.t
        }
      ]
  | `Timeout ->
    error_s
      [%sexp
        "sending signal timed out",
        { pid    : Pid.t
        ; signal : Signal.t
        }
      ]
;;

let process_run ~prog ~args ~timeout () =
  match%bind Process.create ~prog ~args () with
  | Error _ as e -> return e
  | Ok process ->
    match%bind
      Clock.with_timeout timeout (Process.collect_stdout_and_wait process)
    with
    | `Result result -> return result
    | `Timeout ->
      let errors = ref [] in
      let add_error error = errors := error :: !errors in
      add_error
        (Error.create_s
           [%sexp
             "running process timed out",
             { prog : string
             ; args : string list
             }
           ]);
      let pid = Process.pid process in
      let%map () =
        match%map send_sigkill_if_pid_exists pid with
        | Ok () -> ()
        | Error err -> add_error err
      in
      Error (Error.of_list (List.rev !errors))
;;

let exe =
  lazy (
    (* We use the [emacsclient] in the user's PATH rather than an absolute path -- this is
       a client side module and thus this offers more hook for customization from the
       user. *)
    let emacsclient = "emacsclient" in
    match%map
      process_run ()
        ~prog:emacsclient
        ~args:[ "--alternate-editor=/bin/false"
              ; "-e"; "Omake.Server.program"
              ]
        ~timeout:(sec 1.0)
    with
    | Error _ as e -> e
    | Ok stdout ->
      match String.split_lines stdout with
      | [] | _ :: _ :: _ as lines ->
        error (sprintf "strange output from %s" emacsclient) lines
          [%sexp_of: string list]
      | [ line ] ->
        let quote = "\"" in
        let line =
          if String.is_prefix ~prefix:quote line
          then String.chop_prefix_exn ~prefix:quote line
          else line
        in
        let line =
          if String.is_suffix ~suffix:quote line
          then String.chop_suffix_exn ~suffix:quote line
          else line
        in
        Ok line)
;;

module Jenga = struct
  type t =
    { enclosing_repo_root : Repo_root.t
    ; pid : Pid.t
    }
  [@@deriving sexp_of]


  let find_in_enclosing_repo_root enclosing_repo_root =
    let enclosing_repo_path =
      enclosing_repo_root
      |> Repo_root.to_abspath
      |> Abspath.to_string
    in
    let case1 () =
      let module Lock = struct
        type t = { pid : Pid.t }
        [@@deriving sexp]

        let t_of_sexp sexp = Sexp.of_sexp_allow_extra_fields t_of_sexp sexp
      end in
      match%map
        Reader.load_sexp (enclosing_repo_path ^/ ".jenga.lock") Lock.t_of_sexp
      with
      | Error _ -> None
      | Ok { pid } -> Some pid
    in
    let case2 () =
      match%map
        Reader.load_sexp (enclosing_repo_path ^/ ".jenga/lock") Pid.t_of_sexp
      with
      | Error _ -> None
      | Ok pid -> Some pid
    in
    match%map Deferred.List.find_map ~f:(fun f -> f ()) [ case1; case2 ] with
    | None -> None
    | Some pid -> Some { enclosing_repo_root; pid }
  ;;

  let kill t = send_sigkill_if_pid_exists t.pid
  ;;
end

module Project_id = struct
  include String_id.Make (struct let module_name = "Project_id" end) ()

  (** jane-elisp build-manager refers to running builds by their root directory. *)
  let of_jenga (jenga : Jenga.t) =
    of_string (Filename.realpath (Repo_root.to_string jenga.enclosing_repo_root))
end

let kill_project id =
  match%bind force exe with
  | Error _ as e -> return e
  | Ok build_manager_executable ->
    process_run ()
      ~prog:build_manager_executable
      ~args: [ "project"; "kill"; Project_id.to_string id ]
      ~timeout:(sec 1.0)
;;
