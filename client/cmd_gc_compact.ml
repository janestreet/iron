open! Core.Std
open! Async.Std
open! Import

let command =
  Command.async'
    ~summary:"have the server do a gc compaction"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () -> Gc_compact.rpc_to_server_exn ()
    )
;;
