open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"check if locks are unlocked"
    ~readme:(fun () ->
      concat [ "\
[fe is-unlocked] exits status zero if the supplied locks are unlocked. Otherwise, it
exits nonzero and prints a message on stderr.  You must supply at least one lock, or
use ["; Switch.all_locks; "].

To see what is locked for a feature, use:

  $ fe show "; Switch.what_is_locked; " [FEATURE]
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and lock_names = lock_names
     and check_all = no_arg_flag Switch.all_locks ~doc:"check all locks"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let check_lock_names =
         if check_all
         then Lock_name.all
         else (
           (if List.is_empty lock_names
            then failwith "must specify which locks to check");
           lock_names)
       in
       let feature_path = ok_exn feature_path in
       let%map locked = Get_locked.rpc_to_server_exn { feature_path } in
       let locked =
         List.filter locked ~f:(fun (lock_name, _) ->
           List.mem check_lock_names lock_name ~equal:Lock_name.equal)
         |> Get_locked.Reaction.sort
       in
       if not (List.is_empty locked)
       then
         raise_s
           [%sexp "locked", (locked : (Lock_name.t * Feature.Locked.t list) list)])
;;
