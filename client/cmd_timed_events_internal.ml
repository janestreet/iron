open! Core
open! Import

let show =
  Command.async' ~summary:"dump the timed event table state"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () -> Cmd_dump.dump Timed_event_table
    )
;;

let clear_timed_event_errors =
  Command.async'
    ~summary:"clear time event execution errors stored on server"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       With_timed_event_errors.rpc_to_server_exn Clear
    )
;;

let get_timed_event_errors =
  Command.async'
    ~summary:"get time event execution errors stored on server"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       With_timed_event_errors.rpc_to_server_exn Get
    )
;;

let errors_group =
  Command.group ~summary:"deal with time event execution errors stored on server"
    [ "clear" , clear_timed_event_errors
    ; "get"   , get_timed_event_errors
    ]
;;

let command =
  Command.group
    ~summary:"deal with asynchronous events fired at some specific times"
    [ "errors" , errors_group
    ; "show"   , show
    ]
;;
