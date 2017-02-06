open! Core
open! Async
open! Import

include (struct
  open Command.Param
  open Command.Let_syntax

  let user_names =
    let%map user_list = anon (sequence ("USER" %: user_name)) in
    User_name.Set.of_list user_list
  ;;

  let idempotent =
    no_arg_flag "-idempotent"
      ~doc:"do not fail if setting is already as requested"
  ;;
end)

let readme (t : User_set.t) () =
  match t with
  | `Admins -> ""
  | `Feeding_metrics -> ""
  | `Using_locked_sessions ->
    concat [ "\
Some users may opt in here to try out an experimental feature:

When they run [fe review] or [fe session diff] their session is automatically locked,
as if they had run [fe session lock].

If the experience is positive, this may become the new behavior implemented for everyone.
"]
;;

let make_command user_set =
  let change_command change ~summary =
    Command.async' ~summary
      (let open Command.Let_syntax in
       let%map_open () = return ()
       and idempotent = idempotent
       and user_names = user_names
       in
       fun () ->
         if Set.is_empty user_names then failwith "One must supply at least one user.";
         User_set.Change.rpc_to_server_exn
           { user_set
           ; user_names
           ; change
           ; idempotent
           })
  in
  let add_command    = change_command `Add    ~summary:"add users in this set"
  and remove_command = change_command `Remove ~summary:"remove users from this set" in
  let get_command =
    Command.async'
      ~summary:"print out the users of this set"
      (let open Command.Let_syntax in
       let%map_open () = return () in
       fun () ->
         let open! Deferred.Let_syntax in
         let%map users = User_set.Get.rpc_to_server_exn user_set in
         Set.iter users ~f:(fun user ->
           print_endline (User_name.to_string user)))
  in
  Command.group
    ~summary:""
    ~readme:(readme user_set)
    [ "add"    , add_command
    ; "remove" , remove_command
    ; "get"    , get_command
    ]
;;
