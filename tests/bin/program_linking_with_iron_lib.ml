open! Core
open! Async
open! Iron.Std

(* Enforce the linking *)

let _enforce_some_linking =
  let _ = Iron_common.Iron_uuid.create () in
  let _ = Iron_protocol.Feature.feature_id in
  let _ = Iron_hg.Hg.create_rev in
  ()
;;

let command =
  Command.basic' ~summary:"Check that linking with iron does not start async's scheduler"
    (let open! Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       if Async.Scheduler.is_running ()
       then failwith "Async scheduler is running at toplevel";
       if not (Async.Scheduler.is_ready_to_initialize ())
       then failwith "Async scheduler initialized")
;;

let () = Command.run command
