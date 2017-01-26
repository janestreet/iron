open! Core
open! Import

let check_max_exn max =
  if max < 0
  then raise_s [%sexp "invalid max subscriptions, value >= 0 expected", (max : int)]
;;

let set_max_subscriptions_per_user =
  Command.async' ~summary:"set the maximum number of subscriptions per user"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and max = anon ("max" %: int) in
     fun () ->
       check_max_exn max;
       With_event_subscriptions.rpc_to_server_exn (Set_max_subscriptions_per_user max)
    )
;;

let set_max_subscriptions_global =
  Command.async' ~summary:"set the maximum number of subscriptions globally"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and max = anon ("max" %: int)
     in
     fun () ->
       check_max_exn max;
       With_event_subscriptions.rpc_to_server_exn (Set_max_subscriptions_global max)
    )
;;

let drop_all_by_users =
  Command.async' ~summary:"drop all subscriptions opened by a particular user or all users"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and user = for_or_all_required
     in
     fun () ->
       With_event_subscriptions.rpc_to_server_exn (Drop_all_by_user user)
    )
;;

let show =
  Command.async' ~summary:"output the current limits and subscriptions"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () -> Cmd_dump.dump Event_subscriptions
    )
;;

let command =
  Command.group
    ~summary:"deal with long standing connections to Iron which consume asynchronous \
              events"
    ~readme:(fun () ->
      concat [ "\
Operations side effecting the subscriptions require admin privileges.
"])
    [ "drop-all-by-users"              , drop_all_by_users
    ; "set-max-subscriptions-global"   , set_max_subscriptions_global
    ; "set-max-subscriptions-per-user" , set_max_subscriptions_per_user
    ; "show"                           , show
    ]
;;
