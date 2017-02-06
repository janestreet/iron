open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"make fe ask hydra to redo its work for a given feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     in
     fun () ->
       let feature_path = ok_exn feature_path in
       Force_retry.rpc_to_server_exn feature_path
    )
;;
