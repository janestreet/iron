open Core
open Async
open Import

let pause =
  Command.async'
    ~summary:"pause the persistent-state serializer"
    ~readme:(fun () -> "\
This command blocks until the file system is frozen.
")
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and with_timeout =
       let default = Iron_config.serializer_pause_timeout_default in
       flag "-with-timeout"
         (optional_with_default default time_span)
         ~doc:(sprintf
                 "SPAN automatically resume the serializer after SPAN (default %s)"
                 (Time.Span.to_short_string default))
     in
     fun () ->
       Persistent_state_serializer.Pause.rpc_to_server_exn { with_timeout }
    )
;;

let wait_until_synced =
  Command.async'
    ~summary:"wait until prior state changes are reflected in the file system"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       Persistent_state_serializer.Prior_changes_synced_to_file_system.rpc_to_server_exn
         ()
    )
;;

let resume =
  Command.async'
    ~summary:"resume the persistent-state serializer"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       Persistent_state_serializer.Resume.rpc_to_server_exn ()
    )
;;

let status =
  Command.async'
    ~summary:"check whether changes to persistent-state are allowed or not"
    (let open Command.Let_syntax in
     let%map_open () = return () in
     fun () ->
       let open! Deferred.Let_syntax in
       let%map sexp = Persistent_state_serializer.Status.rpc_to_server_exn () in
       printf "%s\n" (Sexp.to_string_hum sexp)
    )
;;

let command =
  Command.group ~summary:"control Iron server's serializer of persistent state"
    ~readme:(fun () -> "\
Iron server maintains its state on the file system via a \"serializer\", which has a queue
of pending changes to be made to the file system to reflect state changes.  Whenever Iron
server commits to a state change, it enqueues a representation of the state change in its
serializer.

The [serializer] commands manipulate the serializer queue.

Iron's backup process uses [pause] and [resume] to get a consistent snapshot of the
persistent state.  The backup process temporarily [pause]s processing of the serializer
queue, copies the state elsewere on disk, and then [resume]s processing of the queue.
")
    [ "pause"            , pause
    ; "resume"           , resume
    ; "wait-until-synced", wait_until_synced
    ; "status"           , status
    ]
;;
