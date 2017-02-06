open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"repartitions crs according to the current aliases and typos"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       Repartition_crs.rpc_to_server_exn ()
    )
;;
