open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"ping the server"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and count =
       flag "-count" (optional_with_default 1 int)
         ~doc:"N number of pings to send"
     and how =
       flag "-how" (optional_with_default `Sequential
                      (Arg_type.of_alist_exn
                         [ ("parallel"  , `Parallel)
                         ; ("sequential", `Sequential)
                         ]))
         ~doc:"{parallel|sequential} send in parallel or in sequence"
     and watch =
       no_arg_flag "-watch"
         ~doc:" ping forever every 2 seconds and print connection status"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       if watch
       then (
         Deferred.forever () (fun () ->
           let%bind result =
             Monitor.try_with_or_error ~extract_exn:true
               (fun () -> Ping.rpc_to_server_exn ())
           in
           print_endline
             (match result with
              | Ok () -> "Ok"
              | Error err -> Error.to_string_hum err);
           Clock.after (sec 2.)
         );
         Deferred.never ())
       else (
         let%map () =
           Deferred.List.iter ~how (List.init count ~f:Fn.id) ~f:(fun _ ->
             Ping.rpc_to_server_exn ())
         in
         print_elapsed [%here]))
;;
