open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"report Iron server's stats"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and kind =
       enum_required "-kind" ~doc:"KIND what kind of stats to report"
         (module Stat.Kind)
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map sexp = Stat.rpc_to_server_exn { kind } in
       printf "%s\n" (Sexp.to_string_hum sexp)
    )
;;

