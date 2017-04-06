open! Core
open! Async
open! Import

let grep repo_root ~aliases =
  let below = Repo_root.relativize_exn repo_root Abspath.program_started_in in
  let%bind manifest = Hg.manifest repo_root `Dirstate in
  let%bind (obligations, obligations_version) =
    Obligations.create (module Hg) ~repo_root ~dirs:(`Below below) ~manifest ~aliases ()
  in
  let cr_comment_format =
    obligations_version
    |> ok_exn
    |> Obligations_version.cr_comment_format
  in
  Cr_comment.grep repo_root cr_comment_format ~below
    ~file_owner:
      (match obligations with
       | Error _ -> const (error_string "broken obligations")
       | Ok obligations -> Obligations.file_owner obligations)
;;

let print_cr_comments
      ~crs
      ~for_or_all
      ~alternate_names
      ~feature_owner
      ~drop_content
      ~xcrs_only =
  let crs =
    List.filter crs ~f:(fun cr ->
      let should_print =
        match for_or_all with
        | `All_users -> true
        | `User user ->
          User_name.equal user
            (Cr_comment.Assignee.user_name (Cr_comment.assignee cr)
               ~feature_owner ~alternate_names)
      in
      should_print && (not xcrs_only || Cr_comment.is_xcr cr))
  in
  let crs_and_attributes = List.map crs ~f:(fun cr -> cr, []) in
  Cr_comment.print_list ~crs_and_attributes ~include_content:(not drop_content)
;;

let print_cr_soons_in_feature ~cr_soons ~drop_content =
  let crs_and_attributes = List.map cr_soons ~f:(fun cr_soon ->
    let cr_comment = Cr_soon_in_feature.cr_comment cr_soon in
    match Cr_soon_in_feature.active_in cr_soon with
    | None -> cr_comment, []
    | Some feature_path ->
      cr_comment, [("active in", Feature_path.to_string feature_path)])
  in
  Cr_comment.print_list ~crs_and_attributes ~include_content:(not drop_content)
;;

let command =
  Command.async'
    ~summary:"display CRs (by default, CRs to work on now)"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and for_or_all               = for_or_all_default_me
     and feature_path             = feature_path_or_current_bookmark
     and should_grep              = no_arg_flag "-grep-current-dir-and-below"
                                      ~doc:"grep the current directory and below"
     and drop_content             = no_arg_flag "-drop-content"
                                      ~doc:"drop the content of the CR and just show \
                                            the position"
     and display_ascii            = display_ascii
     and max_output_columns       = max_output_columns
     and feature_owner            = flag "-owner" (optional user_name)
                                      ~doc:"USER treat USER as the feature owner, \
                                            for unassigned CRs"
     and show_summary_only        = no_arg_flag "-summary"
                                      ~doc:"show counts of CRs in a table"
     and xcrs_only                = no_arg_flag "-xcrs-only" ~doc:"show XCRs only"
     and soon                     = no_arg_flag "-soon" ~doc:"show cr soons only"
     and someday                  = no_arg_flag "-someday" ~doc:"show cr somedays only"
     and include_active_cr_soons  = include_active_cr_soons
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let feature_path = ok_exn feature_path in
       let incompatible option =
         failwith (option ^ " is incompatible with -soon/someday")
       in
       (if soon || someday
        then (
          (if Option.is_some feature_owner then incompatible "-owner");
          (if show_summary_only then incompatible "-summary");
          (if xcrs_only then incompatible "-xcrs-only")));
       (if not soon && include_active_cr_soons
        then
          failwithf "%s is only meaningful with -soon" Switch.include_active_cr_soons ());
       let%bind or_error = Prepare_for_crs.rpc_to_server { feature_path } in
       let owner_for_crs, alternate_names, aliases =
         match or_error with
         (* We want grep to work even when the server is unavailable *)
         | Error _    ->
           User_name.of_string "owner",
           User_name_by_alternate_name.not_available,
           User_name_by_alternate_name.not_available
         | Ok { owner_for_crs; alternate_names; aliases } ->
           owner_for_crs, alternate_names, aliases
       in
       let feature_owner = Option.value feature_owner ~default:owner_for_crs in
       match should_grep with
       | true ->
         let repo_root = ok_exn Repo_root.program_started_in in
         (* Here it's OK to use [current_bookmark] rather than [active_bookmark], because
            if the bookmark is current, then it used to be active at this revision, and
            it still means that we're grepping the right feature. *)
         let%bind bookmark_or_error = Hg.current_bookmark repo_root in
         (match bookmark_or_error with
          | Error _ -> ()
          | Ok bookmark ->
            if not (String.equal (Feature_path.to_string feature_path) bookmark) then
              failwithf "cannot grep for feature '%s' since current bookmark is '%s'"
                (Feature_path.to_string feature_path)
                bookmark
                ());
         let%map { due_now; due_soon; due_someday } = grep repo_root ~aliases in
         let grepped_crs =
           let due_soon = List.map due_soon ~f:Cr_soon.cr_comment in
           match soon, someday with
           | true,  true  -> due_soon @ due_someday
           | true,  false -> due_soon
           | false, true  -> due_someday
           | false, false -> due_now
         in
         if show_summary_only
         then (
           let summary =
             Cr_comment.Summary.create grepped_crs ~feature_owner ~alternate_names
           in
           Option.iter (Cr_comment.Summary.to_ascii_table summary) ~f:(fun ascii_table ->
             print_string (Ascii_table.to_string ascii_table ~display_ascii
                             ~max_output_columns)))
         else
           print_cr_comments ~crs:grepped_crs ~for_or_all ~alternate_names ~feature_owner
             ~drop_content ~xcrs_only
       | false ->
         if someday then
           failwith
             "[fe crs] is incompatible with -someday, use [fe crs -grep -someday]";
         match soon with
         | true ->
           let%map cr_soons =
             Get_cr_soons.rpc_to_server_exn
               { root_feature   = Feature_path.root feature_path
               ; for_or_all
               ; include_active = include_active_cr_soons
               }
           in
           let module CR_soon_in_feature = Cr_soon_multiset.Cr_soon_in_feature in
           let cr_soons =
             List.sort (Cr_soon_multiset.to_list cr_soons)
               ~cmp:Cr_soon_in_feature.For_sorted_output.compare
           in
           print_cr_soons_in_feature ~cr_soons ~drop_content
         | false ->
           if show_summary_only
           then (
             let%map summary =
               Get_cr_summary.rpc_to_server_exn { feature_path }
             in
             let summary = ok_exn summary in
             Option.iter (Cr_comment.Summary.to_ascii_table summary)
               ~f:(fun ascii_table ->
                 print_string (Ascii_table.to_string ascii_table ~display_ascii
                                 ~max_output_columns)))
           else (
             let%map crs =
               Get_crs.rpc_to_server_exn { feature_path; for_or_all }
             in
             let crs = ok_exn crs in
             print_cr_comments ~crs ~for_or_all ~alternate_names ~feature_owner
               ~drop_content ~xcrs_only))
;;
