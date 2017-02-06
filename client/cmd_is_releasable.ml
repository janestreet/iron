open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"check whether a feature is releasable"
    ~readme:(fun () -> "\
Exit status zero if a feature is releasable, else exit nonzero with reasons on stderr.
")
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     in
     fun () ->
       let feature_path = ok_exn feature_path in
       Is_releasable.rpc_to_server_exn { feature_path }
    )
;;
