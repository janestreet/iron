open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"remove aliases from the mapping from aliases to user names"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and aliases = anon (sequence ("ALIAS" %: alias))
     and may_repartition_crs = may_repartition_crs
     in
     fun () ->
       Remove_alternate_names.rpc_to_server_exn
         { which               = `Aliases
         ; alternate_names     = aliases
         ; may_repartition_crs
         }
    )
;;
