open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"watch for updates and dump a feature each time it is changed on the server"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_id = feature_id
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind pipe =
         Notify_on_feature_updates.rpc_to_server_exn
           { feature_id; when_to_first_notify = At_next_change }
       in
       Pipe.iter_without_pushback pipe ~f:(fun result ->
         printf !"%{sexp#mach:Notify_on_feature_updates.Reaction.t}\n"
           (ok_exn result))
    )
;;
