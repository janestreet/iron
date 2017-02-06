open Core
open! Async
open Import

let main_internal ~verbose ~for_
      { Fe.Lock.Action.
        feature_path
      ; lock_names
      ; reason
      ; is_permanent
      } =
  if List.is_empty lock_names then failwith "must supply at least one lock";
  Cmd_change.apply_updates_exn () ~feature_path ~updates:lock_names ~verbose
    ~sexp_of_update:[%sexp_of: Lock_name.t]
    ~rpc_to_server_exn:(fun ~feature_path ~updates:lock_names ->
      Lock_feature.rpc_to_server_exn
        { feature_path
        ; for_
        ; lock_names
        ; reason
        ; is_permanent
        })
;;

let main = main_internal ~verbose:false ~for_:User_name.unix_login

let command =
  let switch_permanent = "-permanent" in
  Command.async'
    ~summary:"lock some of a feature's locks"
    ~readme:(fun () ->
      concat [ "\
[fe lock] can be used to prevent users from performing certain actions on a feature.
For example, [fe lock -release] will prevent users from releasing a feature.

[fe lock] can be used to change the reason for an existing lock or lock new locks.
Modifying existing locks can be done only by the person who locked them.
For example:

  $ fe lock -rebase          -reason 'initial reason'
  $ fe lock -rebase -release -reason 'updated reason'

Locks may be made permanent by supplying [" ; switch_permanent
             ; "] as a way to emphasis that
the lock is intended for a non transient period.  For example, certain features
are not meant to be ever released.

Use [fe unlock] to unlock a lock.  To see what is locked for a feature, use:

  $ fe show "; Switch.what_is_locked; " [FEATURE]
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and for_ = for_
     and lock_names = lock_names
     and reason = lock_reason
     and verbose = verbose
     and is_permanent = no_arg_flag switch_permanent ~doc:" make the lock permanent"
     in
     fun () ->
       let feature_path = ok_exn feature_path in
       main_internal ~verbose ~for_
         { feature_path
         ; lock_names
         ; reason
         ; is_permanent
         }
    )
;;
