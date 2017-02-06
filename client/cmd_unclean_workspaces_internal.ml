open! Core
open! Async
open! Import

let remove_user =
  Command.async'
    ~summary:"clear unclean workspaces associated with a USER on the server"
    ~readme:(fun () -> "\
This command may be run by the USER, otherwise admin privileges are required.
")
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and user = anon ("USER" %: (Arg_type.create User_name.of_string))
     in
     fun () ->
       With_unclean_workspaces.rpc_to_server_exn (Remove_user user)
    )
;;

let command =
  Command.group
    ~summary:"deal with unclean workspaces on the server"
    ~readme:(fun () ->
      concat [ "\
Operations side effecting someone else's state require admin privileges.
"])
    [ "remove-machine", Cmd_has_moved.command
                          ~moved_to:[ "fe"; "workspace"; "unclean"; "remove-machine" ]
    ; "remove-user"   , remove_user
    ]
;;
