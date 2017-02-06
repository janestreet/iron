open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"check the running server"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       Check_invariant.rpc_to_server_exn ()
    )
;;
