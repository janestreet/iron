open Core
open! Async
open Import

let main_internal ~verbose
      { Fe.Unlock.Action.
        feature_path
      ; for_
      ; lock_names
      ; even_if_permanent
      } =
  if List.is_empty lock_names then failwith "must supply at least one lock to unlock";
  Cmd_change.apply_updates_exn () ~feature_path ~updates:lock_names ~verbose
    ~sexp_of_update:[%sexp_of: Lock_name.t]
    ~rpc_to_server_exn:(fun ~feature_path ~updates:lock_names ->
      Unlock_feature.rpc_to_server_exn
        { feature_path
        ; for_
        ; lock_names
        ; even_if_permanent
        })
;;

let main = main_internal ~verbose:false

let command =

  Command.async'
    ~summary:"unlock some of a feature's locks"
    ~readme:(fun () ->
      concat [ "\
[fe unlock] succeeds only if the supplied locks are locked and the user requesting to
unlock is the one who locked them.  You must supply at least one lock.  Unlocking for
someone else is possible using [-for].  If the lock is permanent, it is required to
supply the switch [" ; Switch.even_if_permanent ; "].

To see what is locked for a feature, use:

  $ fe show "; Switch.what_is_locked; " [FEATURE]
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and for_ = for_
     and lock_names =lock_names
     and verbose = verbose
     and even_if_permanent =
       no_arg_flag Switch.even_if_permanent
         ~doc:" allow to unlock even if the lock is permanent"
     in
     fun () ->
       let feature_path = ok_exn feature_path in
       main_internal ~verbose
         { feature_path
         ; for_
         ; lock_names
         ; even_if_permanent
         }
    )
;;
