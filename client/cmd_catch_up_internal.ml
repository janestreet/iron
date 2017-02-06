open Core
open Async
open Import

let mark_id =
  Command.async'
    ~summary:"mark as caught up diffs in a catch-up session"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path         = catch_up_feature_path
     and for_                 = for_
     and catch_up_session_id  = session_id_required
     and diff4_in_session_ids = diff4_in_session_ids
     in
     fun () ->
       let feature_path = ok_exn feature_path in
       Catch_up_diffs.rpc_to_server_exn
         { for_
         ; feature_path
         ; catch_up_session_id
         ; diff4_in_session_ids
         }
    )
;;

let mark_file = Cmd_catch_up.mark_file ~deprecated:true

let show_num_lines =
  Command.async'
    ~summary:"show the number of lines remaining to catch up"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = catch_up_feature_path_or_current_bookmark
     and for_         = for_
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind status =
         Get_catch_up_session.rpc_to_server_exn { feature_path; for_ }
       in
       let lines =
         match status with
         | `Up_to_date -> 0
         | `Catch_up_session { line_count_remaining_to_catch_up; _ } ->
           Line_count.Catch_up.total line_count_remaining_to_catch_up
       in
       print_endline (Int.to_string_hum lines);
       return ()
    )
;;

let command =
  Command.group ~summary: "various catch-up commands for Iron developers and tests"
    [ "mark-id"         , mark_id
    ; "mark-file"       , mark_file
    ; "show-num-lines"  , show_num_lines
    ]
;;
