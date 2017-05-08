open Core
open Async
open Import

let commit =
  Command.async'
    ~summary:"finish the current session, adding reviewed diffs to the brain"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and for_ = for_
     and which_session = which_session
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind () =
         Cmd_review.may_modify_others_review_exn feature_path ~reason:`Not_supported
           ~whose_review:(`User for_)
       in
       let%bind repo_root =
         Monitor.try_with_or_error (fun () ->
           Cmd_workspace.repo_for_hg_operations_exn feature_path
             ~use:`Share_or_clone_if_share_does_not_exist)
       in
       match%bind
         Cmd_review.commit_session_exn repo_root ~feature_path ~for_ ~which_session
           ~display_ascii ~max_output_columns
       with
       | `Committed -> return ()
       | `Cancelled -> Async_interactive.print_endline "Quit"
    )
;;

let diff =
  Command.async'
    ~summary:"show diffs from the current session"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path          = feature_path_or_current_bookmark
     and for_                  = for_
     and may_modify_local_repo = may_modify_local_repo
     and context               = context ()
     and lines_required_to_separate_ddiff_hunks_override =
       lines_required_to_separate_ddiff_hunks_override
     and which_session         = which_session
     and which_diffs           = which_diffs
     and do_not_lock_session =
       no_arg_flag "-do-not-lock-session" ~doc:" do not lock the session"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind repo_root =
         Cmd_workspace.repo_for_hg_operations_exn feature_path ~use:`Clone
       in
       let%bind rev_zero = Hg.create_rev_zero repo_root in
       let%bind { status
                ; remote_repo_path
                ; _
                } =
         Get_review_session.rpc_to_server_exn
           { feature_path
           ; rev_zero         = Some rev_zero
           ; for_
           ; ensure_reviewing = false
           ; which_session
           ; lock_session     = if do_not_lock_session then `No else `If_applicable
           }
       in
       match status with
       | `Up_to_date | `Bookmark_update_is_pending ->
         raise_s
           [%sexp
             "reviewer is up to date, no current session"
           , ((feature_path, for_) : Feature_path.t * User_name.t)
           ]
       | `Review_session
           { Get_review_session.Review_session.
             review_session_tip
           ; reviewer_in_session
           ; diff4s_in_session
           ; lines_required_to_separate_ddiff_hunks
           ; _
           } ->
         let lines_required_to_separate_ddiff_hunks =
           Option.value lines_required_to_separate_ddiff_hunks_override
             ~default:lines_required_to_separate_ddiff_hunks
         in
         let%bind () =
           if not may_modify_local_repo
           then return ()
           else
             Hg.pull repo_root ~from:remote_repo_path
               ~even_if_unclean:true (`Rev review_session_tip)
         in
         let diff4s_in_session =
           diff4s_in_session
           |> Array.map ~f:Diff4_in_session.And_review_kind.diff4_in_session
           |> Array.to_list
         in
         let diff4s_in_session =
           match which_diffs with
           | (`All | `Files _) as which_file ->
             Command.Param.Which_files.restrict_exn which_file diff4s_in_session
               ~path_in_repo:Diff4_in_session.path_in_repo_at_f2
               ~from:"session"
           | ( `Reviewed | `Unreviewed ) as which_file ->
             List.filter diff4s_in_session ~f:(fun diff4_in_session ->
               let is_reviewed = Diff4_in_session.is_reviewed diff4_in_session in
               match which_file with
               | `Reviewed   -> is_reviewed
               | `Unreviewed -> not is_reviewed)
         in
         Cmd_review.print_diff4s
           ~repo_root
           ~diff4s:(List.map diff4s_in_session ~f:Diff4_in_session.diff4)
           ~reviewer:(`Reviewer reviewer_in_session)
           ~context
           ~lines_required_to_separate_ddiff_hunks)
;;

let forget =
  Command.async'
    ~summary:"forget previously reviewed diffs in the current session"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and for_ = for_
     and which_session = which_session
     and files =
       flag "-file" (listed path_in_repo_arg_type)
         ~doc:"FILE forget only specified files"
     and all =
       no_arg_flag "-all" ~doc:"mark all files in the current session as unreviewed"
     and display_ascii = display_ascii
     and max_output_columns = max_output_columns
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let what_to_forget =
         match files, all with
         | [], false -> failwith "must specify one of -file or -all"
         | _ :: _, true ->
           raise_s
             [%sexp
               "-file and -all are incompatible options"
             , (`files (files : Path_in_repo.t list))
             ]
         | [], true -> `All
         | (_ :: _ as files), false -> `Files files
       in
       let feature_path = ok_exn feature_path in
       let%bind () =
         Cmd_review.may_modify_others_review_exn feature_path ~reason:`Not_supported
           ~whose_review:(`User for_)
       in
       let action =
         match what_to_forget with
         | `All     -> "forget"                    (* this review session *)
         | `Files _ -> "forget specified files in" (* this review session *)
       in
       let%bind repo_root =
         Monitor.try_with_or_error (fun () ->
           Cmd_workspace.repo_for_hg_operations_exn feature_path
             ~use:`Share_or_clone_if_share_does_not_exist)
       in
       match%bind
         Cmd_review.confirm_review_session_id_exn repo_root
           ~action ~feature_path ~for_ ~which_session
           ~display_ascii ~max_output_columns
       with
       | `Cancelled ->
         Async_interactive.print_endline "Quit"
       | `Id review_session_id ->
         let%bind () =
           Session.Forget.rpc_to_server_exn
             { Session.Forget.Action.feature_path
             ; for_
             ; review_session_id
             ; what_to_forget
             }
         in
         Async_interactive.printf "%s session: Done\n%!" action)
;;

let set_lock ~set_is_locked_to ~summary ~readme =
  Command.async' ~summary ~readme
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and for_ = for_
     and which_session = which_session
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind () =
         Cmd_review.may_modify_others_review_exn feature_path ~reason:`Not_supported
           ~whose_review:(`User for_)
       in
       Session.Set_lock.rpc_to_server_exn
         { feature_path
         ; for_
         ; which_session
         ; set_is_locked_to
         }
    )
;;

let lock =
  set_lock ~set_is_locked_to:true
    ~summary:"lock the current session, prevent it from being deleted if not reviewed"
    ~readme:(fun () ->
      concat [ "\
When a session is not locked, if it has no reviewed diff and stays idle for a while,
it can be silently deleted on the server and replaced by a session that will make the
user review the files to a more recent changeset in the feature, as the tip advances.

A session may be manually locked, using this command, or automatically locked as soon
as a user look at one of the diff in their session [1].

A locked session may never be deleted even if it does not contain any reviewed
diff.  A session becomes unlocked in any of the following circumstances:

* one runs [fe session unlock]
* one runs [fe session commit]
* all files in the session become reviewed

Locking a session is idempotent, but the command will fail if the user has no current
session, or if someone is trying to act on behalf of someone else without having the
required permissions.

[1] However, as of now the automatic locking is not the default behavior for everyone.
It requires one to be in a special opt-in set. See [fe admin users using-locked-sessions]
"])
;;

let unlock =
  set_lock ~set_is_locked_to:false
    ~summary:"unlock the current session"
    ~readme:(fun () ->
      concat [ "\
One may choose to run this command in order to unlock a session, without committing it.

Unlocking a session is idempotent, but the command will fail if the user has no current
session, or if someone is trying to act on behalf of someone else without having the
required permissions.
"])
;;

let mark_file ~deprecated =
  let subsumed_by = "[fe session mark-file]" in
  Command.async'
    ~summary:"mark some files as reviewed"
    ~readme:(fun () ->
      if deprecated
      then concat [ "\
This command is deprecated and has been subsumed by " ; subsumed_by ; ".
"]
      else ""
    )
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path
     and for_ = for_
     and reason = review_reason
     and which_session = which_session
     and create_catch_up_for_me = create_catch_up_for_me
     and even_if_some_files_are_already_reviewed = even_if_some_files_are_already_reviewed
     and paths_in_repo = paths_in_repo
     in
     fun () ->
       let open! Deferred.Let_syntax in
       if deprecated && am_functional_testing
       then failwithf "This command is deprecated.  \
                       Tests should be updated to use %s instead" subsumed_by ();
       let feature_path = ok_exn feature_path in
       let is_reviewing_for = `User for_ in
       let create_catch_up_for_me =
         create_catch_up_for_me ~is_reviewing_for |> ok_exn
       in
       let%bind () =
         Cmd_review.may_modify_others_review_exn feature_path ~reason:(`This reason)
           ~whose_review:is_reviewing_for
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
         raise_s [%sexp "review is up to date, cannot mark file"
                      , (paths_in_repo : Path_in_repo.t list)]
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
           else Or_error.error_s [%sexp "file not found in the current session"
                                      , (invalid_files : Path_in_repo.t list)]
         in
         let%bind result =
           Reviewed_diffs.rpc_to_server
             { for_
             ; reason
             ; create_catch_up_for_me
             ; even_if_some_files_are_already_reviewed
             ; feature_path
             ; review_session_id
             ; diff4_in_session_ids = List.map ids_to_mark ~f:Diff4_in_session.id
             }
         in
         return (ok_exn (Or_error.combine_errors_unit [ invalid_files ; result ])))
;;

module Attribute = struct
  module T = struct
    type t =
      | Id
      | Tip
      | Is_locked
      | Is_whole_feature_follower
      | Is_whole_feature_reviewer
      | Line_count_from_brain_if_session_was_committed_to_goal
      | Line_count_from_session_end_to_goal
      | Line_count_to_finish_session
      | Lines_required_to_separate_hunks
      | May_be_reviewed_by
      | Num_lines_remaining_to_review_in_session
      | Reviewer
    [@@deriving enumerate, sexp]
  end
  include T
  let sequence =
    Command.Param.enum_no_args (module T)
      ~doc:(fun ~name _ -> sprintf "show %s" name)
  ;;
end

let show =
  Command.async'
    ~summary:"show attributes of a session"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path_or_current_bookmark
     and for_ = for_or_all_default_me
     and attributes = Attribute.sequence
     and as_sexp =
       no_arg_flag "-sexp" ~doc:"print attribute as a record (enforced if attr > 1)"
     and sort_build_order = sort_build_order
     and display_ascii      = display_ascii
     and max_output_columns = max_output_columns
     and which_session      = which_session
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let for_all = match for_ with `All_users -> true | `User _ -> false in
       let%bind repo_root_and_kind_or_error =
         Monitor.try_with_or_error (fun () ->
           Cmd_workspace.repo_for_hg_operations_and_kind_exn feature_path
             ~use:`Share_or_clone_if_share_does_not_exist)
       in
       let show_session ~for_ =
         let%bind { Get_review_session.Reaction. status; _ } =
           Get_review_session.rpc_to_server_exn
             { feature_path
             ; rev_zero               = None
             ; for_
             ; ensure_reviewing       = false
             ; which_session
             ; lock_session           = `No
             }
         in
         match status with
         | `Up_to_date | `Bookmark_update_is_pending ->
           let error =
             Error.create "reviewer is up to date, no current session"
               (feature_path, for_)
               [%sexp_of: Feature_path.t * User_name.t]
           in
           if for_all
           then (print_endline (Error.to_string_hum error); return ())
           else Error.raise error
         | `Review_session
             { Get_review_session.Review_session.
               review_session_id
             ; review_session_tip
             ; reviewer_in_session
             ; reviewer_in_feature
             ; diff4s_in_session
             ; may_be_reviewed_by
             ; line_count_to_finish_session
             ; line_count_to_goal
             ; is_locked
             ; lines_required_to_separate_ddiff_hunks
             } ->
           let diff4s_to_review =
             diff4s_in_session
             |> Array.map ~f:Diff4_to_review.review
           in
           let get_attribute = function
             | Attribute.Id -> review_session_id |> [%sexp_of: Session_id.t]
             | Tip ->
               Rev.node_hash review_session_tip |> [%sexp_of: Node_hash.t]
             | Is_locked -> [%sexp (is_locked : bool)]
             | Is_whole_feature_follower ->
               reviewer_in_session.is_whole_feature_follower |> [%sexp_of: bool]
             | Is_whole_feature_reviewer ->
               reviewer_in_session.is_whole_feature_reviewer |> [%sexp_of: bool]
             | Line_count_from_brain_if_session_was_committed_to_goal ->
               line_count_to_goal.from_brain_if_session_was_committed
               |> [%sexp_of: Line_count.Review.t Or_error.t Or_pending.t]
             | Line_count_to_finish_session ->
               line_count_to_finish_session
               |> [%sexp_of: Line_count.Review.t]
             | May_be_reviewed_by ->
               may_be_reviewed_by |> [%sexp_of: Allow_review_for.Users.t]
             | Line_count_from_session_end_to_goal ->
               line_count_to_goal.from_session_end
               |> [%sexp_of: Line_count.Review.t Or_error.t Or_pending.t]
             | Lines_required_to_separate_hunks ->
               [%sexp (lines_required_to_separate_ddiff_hunks : int)]
             | Num_lines_remaining_to_review_in_session ->
               Line_count.Review.total line_count_to_finish_session
               |> [%sexp_of: int]
             | Reviewer ->
               reviewer_in_session |> [%sexp_of: Reviewer.t]
           in
           (match attributes with
            | [] ->
              let diff4s_to_review = Array.to_list diff4s_to_review in
              let%map diff4s_to_review =
                if not sort_build_order
                then return diff4s_to_review
                else (
                  Build_order.sort repo_root_and_kind_or_error diff4s_to_review
                    Diff4_to_review.path_in_repo_at_f2)
              in
              Cmd_review.print_introduction_summary_for_review
                ~feature_path
                ~review_session_tip
                ~reviewer_in_session
                ~warn_reviewer:
                  (Some
                     { reviewer_in_feature
                     ; line_count_to_finish_session
                     ; line_count_to_goal
                     })
                ~diff4s_to_review
                ~display_ascii
                ~max_output_columns;
            | [ attribute ] when not as_sexp ->
              print_endline (get_attribute attribute |> Sexp.to_string_hum);
              return ();
            | attributes ->
              List.map attributes
                ~f:(fun attribute -> attribute, get_attribute attribute)
              |> [%sexp_of: (Attribute.t * Sexp.t) list]
              |> Sexp.to_string_hum
              |> print_endline;
              return ())
       in
       match for_ with
       | `User user -> show_session ~for_:user
       | `All_users ->
         let%bind reviewers =
           feature_reviewers feature_path ~sort:`Alphabetically
         in
         reviewers
         |> Deferred.List.iter ~how:`Sequential ~f:(fun for_ ->
           print_string (sprintf "%s:\n" (User_name.to_string for_));
           let%map () = show_session ~for_ in
           print_string "\n")
    )
;;

let command =
  Command.group ~summary:"manage your current review session for a feature"
    [ "commit"   , commit
    ; "diff"     , diff
    ; "forget"   , forget
    ; "lock"     , lock
    ; "mark-file", mark_file ~deprecated:false
    ; "show"     , show
    ; "unlock"   , unlock
    ]
;;
