open! Core
open! Async
open! Import

let diff =
  Command.async' ~summary:"show the diff a reviewer knows for a feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and do_not_pull =
       flag "-do-not-pull" no_arg ~doc:" do not pull, fail if revs are unknown"
     and context = context ()
     and feature_path = feature_path_or_current_bookmark
     and for_ = for_
     and which_files = which_files
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind repo_root =
         Cmd_workspace.repo_for_hg_operations_exn feature_path
           ~use:`Share_or_clone_if_share_does_not_exist
       in
       let%bind { brain; reviewer; remote_repo_path; _ } =
         Get_brain.rpc_to_server_exn { feature_path; for_  }
       in
       let brain = List.map brain ~f:(fun d -> d.diff2) in
       let brain =
         Command.Param.Which_files.restrict_exn which_files brain
           ~path_in_repo:Diff2.path_in_repo_at_tip
           ~from:"user's brain"
       in
       let%bind () =
         if do_not_pull
         then return ()
         else (
           let revs =
             let revs = Rev.Compare_by_hash.Hash_set.create () in
             List.iter brain ~f:(fun { base; tip; num_lines_in_diff = _ } ->
               Hash_set.add revs base.rev;
               Hash_set.add revs tip.rev;
             );
             Hash_set.to_list revs
           in
           Hg.pull ~even_if_unclean:true repo_root ~from:remote_repo_path (`Revs revs))
       in
       Cmd_review.print_diff4s
         ~repo_root
         ~diff4s:(List.map brain ~f:Diff4.create_from_scratch_to_diff2)
         ~reviewer:(`Reviewer reviewer)
         ~context
         ~lines_required_to_separate_ddiff_hunks:(2*context))
;;

let show_brain feature_path ~for_ ~what_to_show ~display_ascii
      ~max_output_columns ~repo_root_and_kind_or_error ~sort_build_order =
  let%bind { brain; _ } =
    Get_brain.rpc_to_server_exn { feature_path ; for_  }
  in
  let brain = List.map brain ~f:(fun d -> d.diff2) in
  let brain =
    match what_to_show with
    | `All -> brain
    | `Files files ->
      let files = Path_in_repo.Hash_set.of_list files in
      List.filter brain ~f:(fun diff2 ->
        Hash_set.mem files (Diff2.path_in_repo_at_tip diff2))
  in
  let%map brain =
    if not sort_build_order
    then return brain
    else (
      Build_order.sort repo_root_and_kind_or_error brain Diff2.path_in_repo_at_tip)
  in
  let brain = List.map brain ~f:Diff4.create_from_scratch_to_diff2 in
  print_string
    (Ascii_table.to_string (Diff4.summary brain ~sort:(not sort_build_order))
       ~display_ascii ~max_output_columns);
  brain
;;

let show_brain_for_all_or_user feature_path ~for_ ~what_to_show ~display_ascii
      ~max_output_columns ~repo_root_and_kind_or_error ~sort_build_order =
  let show_brain ~for_ =
    let%map (_ : Diff4.t list) =
      show_brain feature_path
        ~for_ ~what_to_show ~display_ascii ~max_output_columns
        ~repo_root_and_kind_or_error ~sort_build_order
    in
    ()
  in
  match for_ with
  | `User user -> show_brain ~for_:user
  | `All_users ->
    let%bind reviewers = feature_reviewers feature_path ~sort:`Alphabetically in
    reviewers
    |> Deferred.List.iter ~how:`Sequential ~f:(fun for_ ->
      print_string (sprintf "%s:\n" (User_name.to_string for_));
      let%map () = show_brain ~for_ in
      print_string "\n");
;;

let forget =
  Command.async' ~summary:"remove knowledge from a brain"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path       = feature_path_or_current_bookmark
     and for_               = for_
     and files              = flag "-file" (listed path_in_repo_arg_type)
                                ~doc:"FILE forget the diff for FILE in FEATURE"
     and all                = no_arg_flag "-all" ~doc:"forget all diffs in FEATURE"
     and display_ascii      = display_ascii
     and ()                 = interactive
     and max_output_columns = max_output_columns
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind () =
         Cmd_review.may_modify_others_review_exn feature_path ~reason:`Not_supported
           ~whose_review:(`User for_)
       in
       let what_to_forget =
         match all, files with
         | true ,  []  -> `All
         | false, _::_ -> `Files files
         | true , _::_ -> failwith "cannot use both -all and -file";
         | false,  []  -> failwith "must use -all or -file"
       in
       let forget () =
         Brain_forget.rpc_to_server_exn { feature_path; for_; what_to_forget }
       in
       match what_to_forget with
       | `Files _ -> forget ()
       | `All ->
         if not !Async_interactive.interactive
         then forget ()
         else (
           let%bind repo_root_and_kind_or_error =
             Monitor.try_with_or_error (fun () ->
               Cmd_workspace.repo_for_hg_operations_and_kind_exn feature_path
                 ~use:`Share_or_clone_if_share_does_not_exist)
           in
           let%bind brain =
             show_brain feature_path ~for_ ~what_to_show:what_to_forget
               ~display_ascii ~max_output_columns ~repo_root_and_kind_or_error
               ~sort_build_order:false
           in
           if List.is_empty brain
           then
             Async_interactive.print_endline
               "Your brain is already empty; there is nothing to forget."
           else (
             let%bind () = Async_interactive.print_endline "" in
             match%bind
               Async_interactive.ask_yn ~default:false
                 (sprintf "Really forget above diffs for [%s]?"
                    (Feature_path.to_string feature_path))
             with
             | true -> forget ()
             | false -> Async_interactive.print_endline "Quit")))
;;

let show =
  Command.async' ~summary:"show what a reviewer knows about a feature"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and display_ascii      = display_ascii
     and max_output_columns = max_output_columns
     and feature_path       = feature_path_or_current_bookmark
     and for_               = for_or_all_default_me
     and sort_build_order   = sort_build_order
     and what_to_show       = which_files
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let%bind repo_root_and_kind_or_error =
         Monitor.try_with_or_error (fun () ->
           Cmd_workspace.repo_for_hg_operations_and_kind_exn feature_path
             ~use:`Share_or_clone_if_share_does_not_exist)
       in
       show_brain_for_all_or_user
         feature_path ~for_ ~what_to_show ~display_ascii ~max_output_columns
         ~repo_root_and_kind_or_error ~sort_build_order
    )
;;

let command =
  Command.group ~summary:"manage and inspect a reviewer's brain for a feature"
    [ "diff"  , diff
    ; "forget", forget
    ; "show"  , show
    ]
;;
