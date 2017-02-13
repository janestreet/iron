open Core
open Async
open Import

let mark_id =
  Command.async'
    ~summary:"mark some of the diffs as reviewed in the current session"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path           = feature_path
     and for_                   = for_
     and review_session_id      = session_id_required
     and diff4_in_session_ids   = diff4_in_session_ids
     and reason                 = review_reason
     and create_catch_up_for_me = create_catch_up_for_me
     and even_if_some_files_are_already_reviewed = even_if_some_files_are_already_reviewed
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let is_reviewing_for = `User for_ in
       let create_catch_up_for_me =
         create_catch_up_for_me ~is_reviewing_for |> ok_exn
       in
       let%bind () =
         Cmd_review.may_modify_others_review_exn feature_path ~reason:(`This reason)
           ~whose_review:is_reviewing_for
       in
       Reviewed_diffs.rpc_to_server_exn
         { for_
         ; reason
         ; create_catch_up_for_me
         ; even_if_some_files_are_already_reviewed
         ; feature_path
         ; review_session_id
         ; diff4_in_session_ids
         }
    )
;;

let mark_file = Cmd_session.mark_file ~deprecated:true

let show_num_lines =
  Command.async'
    ~summary:"Show the number of lines remaining to review in the session"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and for_ = for_
     and which_session = which_session
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind { status ; _ } =
         Get_review_session.rpc_to_server_exn
           { feature_path
           ; rev_zero = None
           ; for_
           ; ensure_reviewing = false
           ; which_session
           ; lock_session = `No
           }
       in
       let lines =
         match status with
         | `Up_to_date | `Bookmark_update_is_pending -> 0
         | `Review_session { line_count_to_finish_session = x ; _ } ->
           Line_count.Review.total x
       in
       print_endline (Int.to_string_hum lines);
       return ()
    )
;;

let command =
  Command.group ~summary: "various review commands for Iron developers and tests"
    [ "mark-id"        , mark_id
    ; "mark-file"      , mark_file
    ; "show-num-lines" , show_num_lines
    ]
;;
