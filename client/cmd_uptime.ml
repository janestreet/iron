open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"report Iron server's uptime"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map { Server_started_at.Reaction. server_started_at } =
         Server_started_at.rpc_to_server_exn ()
       in
       printf "%s\n"
         (Time.Span.to_short_string (Time.diff (Time.now ()) server_started_at))
    )
;;
