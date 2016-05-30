open! Core.Std
open! Async.Std
open! Import

let add_user_command =
  Command.async'
    ~summary:"add a user in the special set of users using locked sessions"
    ~readme:(fun () -> "acting -for someone requires admin privileges.")
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and for_ = for_ in
     fun () ->
       Users_using_locked_sessions.Change_user.rpc_to_server_exn
         { user_name = for_
         ; change    = `Add
         }
    )
;;

let remove_user_command =
  Command.async'
    ~summary:"remove a user from the special set of users using locked sessions"
    ~readme:(fun () -> "acting -for someone requires admin privileges.")
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and for_ = for_ in
     fun () ->
       Users_using_locked_sessions.Change_user.rpc_to_server_exn
         { user_name = for_
         ; change    = `Remove
         }
    )
;;

let get_users_command =
  Command.async'
    ~summary:"print out the special set of users using locked sessions"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map users = Users_using_locked_sessions.Get.rpc_to_server_exn () in
       Set.iter users ~f:(fun user ->
         print_endline (User_name.to_string user))
    )
;;

let command =
  Command.group
    ~summary:""
    ~readme:(fun () -> concat [ "\
Some users may opt in here to try out an experimental feature:

When they run [fe review] or [fe session diff] their session is automatically locked,
as if they had run [fe session lock].

If the experience is positive, this may become the new behavior implemented for everyone.
"])
    [ "add-user"     , add_user_command
    ; "remove-user"  , remove_user_command
    ; "get-users"    , get_users_command
    ]
;;
