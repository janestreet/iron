open Core.Std
open Async.Std
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
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let create_catch_up_for_me =
         create_catch_up_for_me ~for_or_all:(`User for_) |> ok_exn
       in
       let%bind () =
         Cmd_review.may_modify_others_review_exn feature_path ~reason:(`This reason)
           ~for_or_all:(`User for_)
       in
       Reviewed_diffs.rpc_to_server_exn
         { for_
         ; reason
         ; create_catch_up_for_me
         ; feature_path
         ; review_session_id
         ; diff4_in_session_ids
         }
    )
;;

let mark_file =
  Command.async'
    ~summary:"mark some files as reviewed"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path
     and for_ = for_
     and reason = review_reason
     and which_session = which_session
     and create_catch_up_for_me = create_catch_up_for_me
     and paths_in_repo = paths_in_repo
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let create_catch_up_for_me =
         create_catch_up_for_me ~for_or_all:(`User for_) |> ok_exn
       in
       let%bind () =
         Cmd_review.may_modify_others_review_exn feature_path ~reason:(`This reason)
           ~for_or_all:(`User for_)
       in
       let%bind { status; _ } =
         Get_review_session.rpc_to_server_exn
           { feature_path
           ; rev_zero         = None
           ; for_
           ; ensure_reviewing = true
           ; which_session
           ; lock_session = `No
           }
       in
       match status with
       | `Up_to_date | `Bookmark_update_is_pending ->
         failwiths "review is up to date, cannot mark file" paths_in_repo
           [%sexp_of: Path_in_repo.t list]
       | `Review_session
           { Get_review_session.Review_session.
             review_session_id
           ; diff4s_in_session
           ; _
           } ->
         let table = Path_in_repo.Table.create () in
         Array.iter diff4s_in_session ~f:(fun { diff4_in_session; review_kind = _ } ->
           let key = Diff4_in_session.path_in_repo_at_f2 diff4_in_session in
           Hashtbl.set table ~key ~data:diff4_in_session);
         let ids_to_mark, invalid_files =
           List.partition_map paths_in_repo ~f:(fun path_in_repo ->
             match Hashtbl.find table path_in_repo with
             | None       -> `Snd path_in_repo
             | Some diff4 -> `Fst diff4)
         in
         let invalid_files =
           if List.is_empty invalid_files
           then Ok ()
           else failwiths "file not found in the current session" invalid_files
                  [%sexp_of: Path_in_repo.t list]
         in
         let%bind result =
           Reviewed_diffs.rpc_to_server
             { for_
             ; reason
             ; create_catch_up_for_me
             ; feature_path
             ; review_session_id
             ; diff4_in_session_ids = List.map ids_to_mark ~f:Diff4_in_session.id
             }
         in
         return (ok_exn (Or_error.combine_errors_unit [ invalid_files ; result ]))
    )
;;

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
