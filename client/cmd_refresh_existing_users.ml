open! Core
open! Async
open! Import

let command =
  Command.async' ~summary:"recompute the set of existing users"
    ~readme:(fun () -> "\
This traverses the entire state to rebuild the set of existing users, which is used \
for tab-completion and validating user names supplied on the command line.
")
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       Refresh_existing_users.rpc_to_server_exn ()
    )
;;
