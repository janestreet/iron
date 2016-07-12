open! Core.Std
open! Async.Std
open! Import

include Make_client_config_intf

module Make (X : Config) = struct

  let load_sexps_conv_or_errors file f =
    let f sexp =
      if false then Debug.ams [%here] "f" sexp [%sexp_of: Sexp.t];
      try f sexp
      with
      | Of_sexp_error _ as exn -> raise exn
      | exn -> raise (Of_sexp_error (exn, sexp))
    in
    if false then Debug.ams [%here] "loading" file [%sexp_of: string];
    Sexplib.Macro.load_sexps_conv file f
  ;;

  let blocking_load_files files =
    let x = X.create () in
    let errors = ref [] in
    let add_error error = errors := error :: !errors in
    List.iter files ~f:(fun file ->
      Or_error.try_with (fun () ->
        let file = Abspath.to_string file in
        if Core.Std.Sys.file_exists_exn file
        then begin
          (* That way we get located errors happening either in [t_of_sexp] or [update] *)
          let execute sexp = X.update x (X.Statement.t_of_sexp sexp) in
          List.iter (load_sexps_conv_or_errors file execute) ~f:(function
            | `Result () -> ()
            | `Error (exc, annot) ->
              add_error (Error.of_exn (Sexp.Annotated.get_conv_exn ~file ~exc annot)))
        end)
      |> function
      | Ok () -> ()
      | Error error ->
        add_error
          (Error.tag_arg error "uncaught exception while loading file"
             file [%sexp_of: Abspath.t])
    );
    x, List.rev !errors
  ;;

  let default_files = lazy begin
    X.always_loaded_if_present
    @ (if not Iron_options.load_user_configs
       then []
       else
         match Core.Std.Sys.getenv "HOME" with
         | None -> []
         | Some home -> [ Abspath.of_string (home ^/ X.home_basename) ])
  end
  ;;

  let default_files_doc = lazy begin
    List.map ~f:Abspath.to_string X.always_loaded_if_present
    @ [ "$HOME/" ^ X.home_basename ]
  end
  ;;

  let blocking_load_and_memoize_exn = lazy begin
    blocking_load_files (force default_files)
  end
  ;;

  let%test_unit _ =
    (* We depends on [load_sexps_conv_or_errors] to catch exceptions and returns location,
       so we better make sure it stays that way or better, gets cleaned up *)
    let open Core.Std in
    let file = Filename.temp_file "make_client_config" "unit_test" in
    Out_channel.write_all file ~data:"()";
    let module A = struct
      exception E
    end in
    let res = load_sexps_conv_or_errors file (fun _ -> raise A.E) in
    (try Unix.unlink file with _ -> ());
    (match res with
     (* | [ `Error (A.E,_) ] -> () *)
     | [ `Error (_, _) ] -> ()
     | _ ->
       let map = function
         | `Result () -> Ok ()
         | `Error (exc, annot) -> Error (Sexp.Annotated.get_conv_exn ~file ~exc annot)
       in
       failwiths "unexpected" (List.map ~f:map res)
         [%sexp_of: (unit, Exn.t) Result.t list])
  ;;

  let get () =
    fst (Lazy.force blocking_load_and_memoize_exn)
  ;;

  let errors () =
    snd (Lazy.force blocking_load_and_memoize_exn)
  ;;

  let validate_config =
    Command.async_or_error'
      ~summary:(sprintf "Validate config in $HOME/%s if it exists" X.home_basename)
      ~readme:(fun () -> concat [ "\
By default the command will validate the following files in the order given,
if each of them exists:\n\n  " ; concat ~sep:"\n  " (force default_files_doc) ; "\
\n\nOne can supply custom files to be validated instead, in which case the files
are expected to be found.
"])
      (let open Command.Let_syntax in
       let%map_open () = return ()
       and files = anon (sequence ("FILE" %: file)) in
       fun () ->
         let open! Deferred.Let_syntax in
         Monitor.try_with_or_error ~extract_exn:true (fun () ->
           let%bind files =
             if List.is_empty files
             then return (force default_files)
             else
               let%map files = Deferred.List.map files ~f:(fun file ->
                 let file =
                   Path.resolve_relative_to_program_started_in (Path.of_string file)
                 in
                 match%map Abspath.file_exists_exn file with
                 | true  -> Ok file
                 | false -> Or_error.error "file not found" file [%sexp_of: Abspath.t])
               in
               files
               |> Or_error.combine_errors
               |> ok_exn
           in
           let errors = snd (blocking_load_files files) in
           List.iter errors ~f:(fun exn ->
             eprintf "%s\n" (Error.to_string_hum exn)
           );
           shutdown (if List.is_empty errors then 0 else 1);
           never ())
      )
  ;;
end

module Utils = struct
  type t = Sexp.t list -> unit

  let empty sexps =
    if not (List.is_empty sexps)
    then failwiths "invalid arguments" sexps [%sexp_of: Sexp.t list]
  ;;

  type flag = Sexp.t list -> Sexp.t list

  let (+>) f flag sexps = f (flag sexps)

  let no_arg flag f sexps =
    List.filter sexps ~f:(function
      | Sexp.Atom str when String.equal str flag ->
        f ();
        false
      | Sexp.Atom _ | Sexp.List _ -> true
    )
  ;;

  let flag flag param_of_sexp f sexps =
    let rec filter rev = function
      | ([] | [ _ ]) -> sexps
      |  Sexp.Atom str :: arg :: tl when String.equal flag str ->
        let value =
          try param_of_sexp arg
          with exn ->
            raise_s
              [%sexp
                "exception while parsing sexp",
                { flag = (flag : string)
                ; sexp = (arg  : Sexp.t)
                ; exn  = (exn  : Exn.t)
                }
              ]
        in
        f value;
        List.rev_append rev tl
      | hd :: tl -> filter (hd::rev) tl
    in
    filter [] sexps
  ;;
end
