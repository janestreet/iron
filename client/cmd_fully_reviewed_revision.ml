open! Core
open! Async
open! Import

let add =
  Command.async' ~summary:"add a fully-reviewed revision"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and rev = rev in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind rev = Raw_rev.resolve_exn rev ~in_:Repo_root.program_started_in in
       Change_fully_reviewed_revisions.rpc_to_server_exn { what_to_do = Add rev }
    )
;;

let check =
  Command.async' ~summary:"check whether a revision is fully-reviewed"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and rev = rev in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind rev = Raw_rev.resolve_exn rev ~in_:Repo_root.program_started_in in
       Revision_is_fully_reviewed.rpc_to_server_exn { rev }
    )
;;

let remove =
  Command.async' ~summary:"remove a fully-reviewed revision"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and rev = rev in
     fun () ->
       let open! Deferred.Let_syntax in
       let%bind rev = Raw_rev.resolve_exn rev ~in_:Repo_root.program_started_in in
       Change_fully_reviewed_revisions.rpc_to_server_exn { what_to_do = Remove rev }
    )
;;

let internal =
  Command.group ~summary:"change the set of fully-reviewed revisions"
    [ "add"   , add
    ; "remove", remove
    ]
;;

let tools =
  Command.group ~summary:"check the set of fully-reviewed revisions"
    [ "check" , check
    ]
;;
