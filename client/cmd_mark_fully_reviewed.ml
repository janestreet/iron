open! Core
open! Async
open! Import

let command =
  Command.async'
    ~summary:"mark a feature as fully reviewed"
    ~readme:(fun () ->
      concat ["\
Catchup review will be generated for everyone but yourself (unless you supply
"; Switch.create_catch_up_for_me; ").

When base or tip are provided, server will check if they match server's knowledge
and only continue when they totally match.  If a 40-character hg revision is provided
then this command works without a local hg repo.
"
             ])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = feature_path
     and whom_to_mark = for_or_all_or_all_but_default_me
     and reason = review_reason
     and create_catch_up_for_me = create_catch_up_for_me
     and base =
       flag "base" (optional rev_from_string_40_or_local_repo_arg_type)
         ~doc:"REV base of the feature you know"
     and tip =
       flag "tip"  (optional rev_from_string_40_or_local_repo_arg_type)
         ~doc:"REV tip of the feature you know"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let whom_to_mark = ok_exn whom_to_mark in
       let%bind repo_root =
         Cmd_workspace.repo_for_hg_operations_exn feature_path ~use:`Clone
       in
       let%bind base = Raw_rev.resolve_opt_exn base ~in_:(Ok repo_root) in
       let%bind tip = Raw_rev.resolve_opt_exn tip  ~in_:(Ok repo_root) in
       let create_catch_up_for_me =
         create_catch_up_for_me ~is_reviewing_for:whom_to_mark |> ok_exn
       in
       let%bind () =
         Cmd_review.may_modify_others_review_exn feature_path ~reason:(`This reason)
           ~whose_review:whom_to_mark
       in
       Mark_fully_reviewed.rpc_to_server_exn
         { feature_path; whom_to_mark; reason; create_catch_up_for_me; base; tip })
;;
