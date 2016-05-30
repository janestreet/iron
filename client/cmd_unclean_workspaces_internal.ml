open! Core.Std
open! Async.Std
open! Import

let remove_machine =
  Command.async'
    ~summary:"clear unclean workspaces associated with a pair USER * MACHINE on the server"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and for_ = for_
     and machine = anon ("MACHINE" %: (Arg_type.create Machine.of_string))
     in
     fun () ->
       With_unclean_workspaces.rpc_to_server_exn (Remove_machine (for_, machine))
    )
;;

let remove_user =
  Command.async'
    ~summary:"clear unclean workspaces associated with a USER on the server"
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
Operations side effecting someone else's state need to be run by a Iron trusted user.
"])
    [ "remove-machine", remove_machine
    ; "remove-user"   , remove_user
    ]
;;
