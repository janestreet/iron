open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"remove typos from the mapping from typo to user name"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and typos = anon (sequence ("TYPO" %: typo))
     and may_repartition_crs = may_repartition_crs
     in
     fun () ->
       Remove_alternate_names.rpc_to_server_exn
         { which               = `Typos
         ; alternate_names     = typos
         ; may_repartition_crs
         }
    )
;;
