open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"watch for updates to a feature and its descendants, \
              reflect each update on stdout as they happen"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind pipe =
         Notify_on_descendant_updates.rpc_to_server_exn
           { feature_path; when_to_first_notify = At_next_change }
       in
       Pipe.iter_without_pushback pipe ~f:(fun result ->
         printf !"%{sexp#mach:Notify_on_descendant_updates.Reaction.t}\n"
           (ok_exn result))
    )
;;
