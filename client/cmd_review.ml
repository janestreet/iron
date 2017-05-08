open! Core
open! Async
open! Import

let fast_cat_or_empty fast_hg_cat_table attributed_file =
  let rev = Attributed_file.rev attributed_file in
  let cached_file =
    let open Option.Let_syntax in
    let path_in_repo = Attributed_file.path_in_repo attributed_file in
    let%bind path_map = Hashtbl.find fast_hg_cat_table rev in
    Map.find path_map path_in_repo
  in
  match cached_file with
  | None -> return ""
  | Some contents -> Lazy_deferred.force_exn contents
;;

let forget_hunks_of_diff4 ~context ~rev_names ~file_names ~header_file_name
      ~scrutiny fast_hg_cat_table diff4 =
  let open Pdiff4.Std in
  match Diff4.is_forget diff4 with
  | false -> return None
  | true ->
    let files = Diff4.diamond diff4 in
    let contents = fast_cat_or_empty fast_hg_cat_table in
    let%bind contents = Diamond.Deferred.all (Diamond.map files ~f:contents) in
    let lines =
      { Diamond.
        b1 = String.split_lines contents.b1
      ; f1 = String.split_lines contents.f1
      ; b2 = []
      ; f2 = []
      }
    in
    let slices =
      let file_names = Diamond.pretty_short_rev_names ~equal:String.equal rev_names in
      Diamond.map2 file_names lines ~f:(fun source lines ->
        Slice.create ~source 0 lines)
    in
    let hunk =
      { Hunk.
        header_file_name
      ; scrutiny
      ; rev_names
      ; file_names
      ; diff4_class = `conflict
      ; views = [ Diff_algo.display_forget ~context slices ]
      ; view_ids_shown = All
      }
    in
    return (Some hunk)
;;

let code_change_hunks_of_diff4 ~reviewer ~context ~lines_required_to_separate_ddiff_hunks
      ~rev_names ~file_names ~header_file_name
      fast_hg_cat_table diff4 =
  let scrutiny =
    match (Diamond.f2 diff4.Diff4.diamond).Attributed_file.attributes with
    | `Absent -> None
    | `Present attributes -> Some attributes.scrutiny
  in
  match%bind
    forget_hunks_of_diff4 ~context ~rev_names ~file_names ~header_file_name
      ~scrutiny fast_hg_cat_table diff4
  with
  | Some forget -> return [ forget ]
  | None ->
    let files = Diff4.diamond diff4 in
    (* Optimization & work around unfortunate hunk split scenarios *)
    let files_diff4_class =
      let hashes = Diamond.map files ~f:(fun file ->
        match file.Attributed_file.attributes with
        | `Absent -> File_contents_hash.of_empty_string
        | `Present attributes -> attributes.hash)
      in
      Diamond.classify ~equal:File_contents_hash.(=) hashes
    in
    if not (Diff4_class.is_shown files_diff4_class)
    || (match reviewer with
      | `Whole_diff_plus_ignored -> false
      | `Reviewer reviewer ->
        match Diff4.may_review diff4 reviewer with
        | `Dropped_from_follow
        | `Dropped_from_review
        | `Review_ownership_change -> true
        | `Nothing_to_review_or_follow
        | `Follow_lines
        | `Review_lines -> false)
    then return []
    else (
      let contents = fast_cat_or_empty fast_hg_cat_table in
      let%bind contents = Diamond.Deferred.all (Diamond.map files ~f:contents) in
      Pdiff4.Std.Patdiff4.hunks
        ~verbose:false
        ~view_ids_computed:Compute_every_view_available
        (* So that the user may navigate among them *)
        ~context
        ~lines_required_to_separate_ddiff_hunks
        ~rev_names ~file_names ~header_file_name
        ~scrutiny ~contents ()
      |> return)
;;

let attributes_contents ~show_file_followers (attributed_file : Attributed_file.t) =
  match attributed_file.attributes with
  | `Absent -> "<absent>"
  | `Present attributes ->
    let { Attributed_file.Attributes.
          hash = _
        ; owner
        ; review_obligation
        ; followers                          = file_followers
        ; scrutiny
        ; is_read_by_whole_feature_reviewers = _
        ; num_lines                          = _
        } = attributes
    in
    let reviewed_by =
      Review_obligation.to_string_hum (module Reviewed_by) review_obligation
    in
    let file_followers =
      match show_file_followers with
      | `No -> []
      | `All ->
        if Set.is_empty file_followers
        then []
        else
          [ "followed by" , Set.to_list file_followers
                            |> List.map ~f:User_name.to_string
                            |> String.concat ~sep:", "
          ]
      | `For user_name ->
        let is_follower = Set.mem file_followers user_name in
        (* We use a special display here for showing file followers changes.  All the
           follower should care about is whether they follow the file. *)
        [ sprintf "followed by %s" (User_name.to_string user_name)
        , if is_follower then "yes" else "no"
        ]
    in
    List.concat
      [ [ "file"        , Path_in_repo.to_string attributed_file.path_in_repo
        ; "scrutiny"    , File_scrutiny.to_string_hum scrutiny
        ; "owner"       , User_name.to_string owner
        ; "reviewed by" , reviewed_by
        ]
      ; file_followers
      ]
    |> Pdiff4.Std.Hunk.align_alist
    |> String.concat ~sep:"\n"
;;

let attributes_change_hunks_of_diff4
      ~reviewer ~rev_names ~file_names ~header_file_name diff4 =
  let show_file_followers =
    match reviewer with
    | `Whole_diff_plus_ignored -> `All
    | `Reviewer (reviewer : Reviewer.t) ->
      if Diamond.exists (Diff4.diamond diff4) ~f:(fun attributed_file ->
        match Attributed_file.attributes attributed_file with
        | `Absent -> false
        | `Present attributes -> Set.mem attributes.followers reviewer.user_name)
      then `For reviewer.user_name
      else `No
  in
  let contents =
    Diamond.map (Diff4.diamond diff4) ~f:(attributes_contents ~show_file_followers)
  in
  Pdiff4.Std.Patdiff4.hunks ~verbose:false
    ~context:Pdiff4.Std.Diff_algo.infinite_context
    ~lines_required_to_separate_ddiff_hunks:Pdiff4.Std.Diff_algo.infinite_context
    ~rev_names ~file_names ~header_file_name
    ~scrutiny:None ~contents ()
;;

let errors_hunks_of_diff4 ~rev_names ~file_names ~header_file_name diff4 =
  let module Diff_algo = Pdiff4.Std.Diff_algo in
  let module Hunk = Pdiff4.Std.Hunk in
  Option.map (Diff_algo.display_errors diff4.Diff4.errors) ~f:(fun view ->
    { Hunk.
      header_file_name
    ; scrutiny = None
    ; rev_names
    ; file_names
    ; diff4_class = `conflict
    ; views = [ view ]
    ; view_ids_shown = All
    })
;;

let hunks_of_diff4 ~reviewer ~context ~lines_required_to_separate_ddiff_hunks fast_hg_cat_table diff4 =
  let files = Diff4.diamond diff4 in
  let rev_names =
    Diamond.map files ~f:(fun file ->
      Attributed_file.rev file
      |> Rev.to_string_12)
  in
  let file_names =
    Diamond.map files ~f:(fun file ->
      if Attributed_file.is_present file
      then Path_in_repo.to_string (Attributed_file.path_in_repo file)
      else "<absent>")
  in
  let header_file_name = Path_in_repo.to_string (Diff4.path_in_repo_at_f2 diff4) in
  let attributes =
    attributes_change_hunks_of_diff4
      ~reviewer ~rev_names ~file_names ~header_file_name diff4
  in
  let errors =
    errors_hunks_of_diff4 ~rev_names ~file_names ~header_file_name diff4
    |> function
    | Some hunk -> [ hunk ]
    | None -> []
  in
  let code =
    code_change_hunks_of_diff4 ~reviewer ~context ~lines_required_to_separate_ddiff_hunks
      ~rev_names ~file_names ~header_file_name
      fast_hg_cat_table diff4
  in
  let%map code = code in
  List.concat
    [ attributes
    ; errors
    ; code
    ]
;;

let pull_and_update_for_catch_up_on_archived_feature
      ~repo_root ~remote_repo_path ~review_session_tip
  =
  (* For catch-up we do not intend for people to edit anything, so the logic is
     significantly simpler.  We do not expect the bookmark to exist (it might exist as a
     different feature) so we have to stick to revisions. *)
  let%bind () =
    Hg.pull repo_root ~from:remote_repo_path
      ~even_if_unclean:true (`Rev review_session_tip)
  in
  let%bind () =
    match%bind Hg.status_cleanliness repo_root with
    | Error _ -> Async_interactive.printf "Repo is not clean.  Not updating.\n"
    | Ok repo_is_clean ->
      Hg.update repo_root (`Rev review_session_tip)
        ~clean_after_update:(Yes repo_is_clean)
  in
  Cmd_workspace.If_enabled.update_satellite_repos ~center_repo_root:repo_root
;;

let cannot_pull cleanliness_error =
  Error.create_s [%sexp "Iron needs to pull but won't", (cleanliness_error : Error.t)]
;;

let pull_and_update
      ~repo_root ~remote_repo_path ~feature_path ~review_session_tip
      ~feature_tip:server_feature_tip
  =
  let is_known_and_is_ancestor ~ancestor ~descendant =
    match%bind Hg.rev_exists repo_root ancestor with
    | false -> return `Unknown
    | true ->
      match%map Hg.is_ancestor repo_root ~ancestor ~descendant with
      | true  -> `Is_ancestor
      | false -> `Is_not_ancestor
  in
  let get_local_feature_tip () =
    Hg.create_rev repo_root (Revset.feature_tip feature_path)
  in
  let is_clean = Lazy_deferred.create (fun () -> Hg.status_cleanliness repo_root) in
  let%bind local_feature_tip_or_error = get_local_feature_tip () in
  let%bind server_tip_is_ancestor_of_local_bookmark =
    match local_feature_tip_or_error with
    | Error _ -> return `No_local_feature
    | Ok local_feature_tip ->
      is_known_and_is_ancestor ~ancestor:server_feature_tip ~descendant:local_feature_tip
  in
  let%bind local_feature_tip =
    match server_tip_is_ancestor_of_local_bookmark with
    | `Is_ancestor -> return local_feature_tip_or_error
    | `Is_not_ancestor | `Unknown | `No_local_feature ->
      match%bind Lazy_deferred.force_exn is_clean with
      | Error cleanliness_error ->
        return
          (match local_feature_tip_or_error with
           | Ok _ as ok -> ok (* We can't pull, so try to live with where we are. *)
           | Error _ -> Error (cannot_pull cleanliness_error))
      | Ok repo_is_clean ->
        let%bind () =
          Hg.pull ~repo_is_clean repo_root (`Feature feature_path)
            ~from:remote_repo_path
        in
        get_local_feature_tip ()
  in
  let local_feature_tip = ok_exn local_feature_tip in
  let%bind () =
    match server_tip_is_ancestor_of_local_bookmark with
    | `Is_ancestor -> return ()
    | `Is_not_ancestor | `Unknown | `No_local_feature ->
      let%map review_session_tip_is_ancestor_of_local_bookmark =
        is_known_and_is_ancestor
          ~ancestor:review_session_tip ~descendant:local_feature_tip
      in
      match review_session_tip_is_ancestor_of_local_bookmark with
      | `Is_ancestor -> ()
      | (`Unknown | `Is_not_ancestor) as status ->
        let ancestor_error =
          match status with
          | `Unknown ->
            Error.create "unknown review session tip.  Consider reviewing to tip"
              review_session_tip [%sexp_of: Rev.t]
          | `Is_not_ancestor ->
            Error.of_string "review-session tip is not an ancestor of local feature tip"
        in
        match Lazy_deferred.peek_exn is_clean with
        | None | Some (Ok Repo_is_clean) -> Error.raise ancestor_error
        | Some (Error cleanliness_error) -> Error.raise (cannot_pull cleanliness_error)
  in
  let%bind () =
    let%bind feature_is_active_bookmark =
      (* Here we must use [active_bookmark] and not [current_bookmark], because we want to
         ensure that if the user edits and commits, they do so with the right bookmark. *)
      match%map Hg.active_bookmark repo_root with
      | Error _ -> false
      | Ok active_bookmark ->
        String.equal active_bookmark (Feature_path.to_string feature_path)
    in
    if feature_is_active_bookmark
    then Deferred.unit
    else (
      match%bind Lazy_deferred.force_exn is_clean with
      | Error cleanliness_error ->
        raise_s [%sexp "Iron needs to [hg update] but won't"
                     , (cleanliness_error : Error.t)]
      | Ok repo_is_clean ->
        Hg.update repo_root (`Feature feature_path)
          ~clean_after_update:(Yes repo_is_clean))
  in
  Cmd_workspace.If_enabled.update_satellite_repos ~center_repo_root:repo_root
;;

let paths_in_repo_by_rev diff4s =
  let paths_in_repo = Path_in_repo.Hash_set.create () in
  let table = Rev.Compare_by_hash.Table.create () in
  List.iter diff4s ~f:(fun diff4 ->
    List.iter (Diff4.present_paths_in_repo diff4) ~f:(fun (rev, path) ->
      let paths_set =
        Hashtbl.find_or_add table rev ~default:Path_in_repo.Hash_set.create
      in
      Hash_set.add paths_set path;
      Hash_set.add paths_in_repo path));
  table
;;

module Account_for_reviewer_warnings = struct
  type t =
    { reviewer_in_feature : Reviewer.t
    ; line_count_to_finish_session : Line_count.Review.t
    ; line_count_to_goal : Line_count.Review.t Or_error.t Or_pending.t
                             Get_review_session.Line_count_to_goal.t
    }
end

let print_introduction_summary_for_review
      ~feature_path
      ~review_session_tip
      ~reviewer_in_session
      ~warn_reviewer
      ~diff4s_to_review
      ~display_ascii
      ~max_output_columns
  =
  (match (warn_reviewer : Account_for_reviewer_warnings.t option) with
   | None -> ()
   | Some { reviewer_in_feature
          ; line_count_to_finish_session
          ; line_count_to_goal
          } ->
     (if not (Reviewer.equal reviewer_in_session reviewer_in_feature)
      then (
        Print.printf "\
Warning: what you need to review may have changed since this session was created.
Consider committing or forgetting your current session:
";
        let table =
          let rows =
            let rows = ref [] in
            let module S = struct
              module type S = sig
                type t
                include Equal.S      with type t := t
                include Stringable.S with type t := t
              end
            end in
            let check (type a) title (module M : S.S with type t = a) field =
              let in_session = Field.get field reviewer_in_session in
              let in_feature = Field.get field reviewer_in_feature in
              if not (M.equal in_session in_feature)
              then
                rows := (title, M.to_string in_session, M.to_string in_feature) :: !rows
            in
            Reviewer.Fields.iter
              ~user_name:(check "user name" (module User_name))
              ~is_whole_feature_reviewer:
                (check "is whole-feature reviewer" (module Bool))
              ~is_whole_feature_follower:
                (check "is whole-feature follower" (module Bool));
            List.rev !rows
          in
          let columns =
            Ascii_table.Column.(
              [ string ~header:"reviewer"   (cell (fun (value, _, _) -> value))
              ; string ~header:"in session" (cell (fun (_, value, _) -> value))
              ; string ~header:"in feature" (cell (fun (_, _, value) -> value))
              ])
          in
          Ascii_table.create ~columns ~rows
        in
        Cmd_show.print_table table ~display_ascii ~max_output_columns;
        Print.printf "\n"));
     (if Client_config.(get () |> show_commit_session_warning)
      then (
        match line_count_to_goal.from_session_end
            , line_count_to_goal.from_brain_if_session_was_committed
        with
        | Known (Ok line_count_from_session_end_to_goal)
        , Known (Ok line_count_from_brain_if_session_was_committed_to_goal) ->
          let num_lines_from_session_end_to_goal_that_must_be_reviewed =
            line_count_from_session_end_to_goal.must_review
          in
          let num_lines_that_would_remain_if_the_session_was_committed =
            line_count_from_brain_if_session_was_committed_to_goal.must_review
          in
          let num_lines_in_session_that_must_be_reviewed =
            Line_count.Review.must_review line_count_to_finish_session
          in
          if num_lines_from_session_end_to_goal_that_must_be_reviewed > 0
          || (num_lines_that_would_remain_if_the_session_was_committed <
              num_lines_in_session_that_must_be_reviewed
              + num_lines_from_session_end_to_goal_that_must_be_reviewed)
          then (
            Print.printf "\
Warning: the feature has changed since this session was created.  It may be more suitable
to review the feature to its most recent tip.  Consider committing your session:
";
            let table =
              let columns =
                Ascii_table.Column.(
                  let int = int ~show_zero:true in
                  [ int ~header:"remaining in session"
                      (cell (const num_lines_in_session_that_must_be_reviewed))
                  ; int ~header:"session end to tip"
                      (cell
                         (const num_lines_from_session_end_to_goal_that_must_be_reviewed))
                  ; int ~header:"remaining if commit"
                      (cell
                         (const num_lines_that_would_remain_if_the_session_was_committed))
                  ])
              in
              Ascii_table.create ~columns ~rows:[()]
            in
            Cmd_show.print_table table ~display_ascii ~max_output_columns;
            Print.printf "\n";
          );
        | _ -> ())));
  let count_total = List.length diff4s_to_review in
  let count_reviewed =
    List.count diff4s_to_review ~f:Diff4_to_review.is_reviewed
  in
  Print.printf "Reviewing %s to %s.\n"
    (Feature_path.to_string feature_path)
    (Rev.to_string_12 review_session_tip);
  let num_lines_in_diff diff4_to_review =
    Diff4_to_review.num_lines diff4_to_review reviewer_in_session
  in
  let total_lines_for_this_session =
    List.sum (module Int) diff4s_to_review ~f:num_lines_in_diff
  in
  let int_size =
    List.fold diff4s_to_review ~init:0 ~f:(fun acc diff4_to_review ->
      num_lines_in_diff diff4_to_review
      |> Int.to_string_hum
      |> String.length
      |> max acc)
  in
  let int_to_string_padding int =
    sprintf "%*s" int_size (Int.to_string_hum int)
  in
  Print.printf "%d files to review%s: %d lines total\n"
    (count_total - count_reviewed)
    (if count_reviewed > 0
     then sprintf " (%d already reviewed)" count_reviewed
     else "")
    total_lines_for_this_session;
  let diff4s_to_review =
    Diff4_to_review.by_kind_of_decreasing_priority diff4s_to_review
  in
  let should_print_kinds =
    match diff4s_to_review with
    | [ Review Must_review, _ ] -> false
    | _ -> true
  in
  List.iter diff4s_to_review ~f:(fun (kind, diff4s_to_review) ->
    if should_print_kinds
    then Print.printf "\n%s\n" (Diff4_to_review.Kind.to_string_hum kind);
    List.iter diff4s_to_review ~f:(fun diff4_to_review ->
      let path_in_repo = Diff4_to_review.path_in_repo_at_f2 diff4_to_review in
      Print.printf "   [%c] %s %s\n"
        (if Diff4_to_review.is_reviewed diff4_to_review
         then 'X'
         else ' ')
        (int_to_string_padding (num_lines_in_diff diff4_to_review))
        (Path_in_repo.to_string path_in_repo)
    ));
;;

let is_testing_client_raises_creating_hunks () =
  am_functional_testing
  && Option.is_some (Sys.getenv "IRON_FUNCTIONAL_TESTING_CLIENT_RAISES_CREATING_HUNKS")
;;

let try_with_and_ignore_subsequent_errors f =
  Deferred.create (fun ivar ->
    don't_wait_for
      (Monitor.handle_errors
         (fun () ->
            let%map res = f () in
            Ivar.fill_if_empty ivar (Ok res))
         (fun exn -> Ivar.fill_if_empty ivar (Error (Error.of_exn exn)))))
;;

let create_files_for_review
      ~temp_dir
      ~repo_root
      ~diff4s
      ~reviewer
      ~context
      ~lines_required_to_separate_ddiff_hunks
  =
  let paths_in_repo_by_rev = paths_in_repo_by_rev diff4s in
  let%map paths_in_repo_by_rev =
    Deferred.List.map ~how:`Parallel (Hashtbl.to_alist paths_in_repo_by_rev)
      ~f:(fun (rev, paths) ->
        let%map map =
          Hg.cat repo_root rev (Hash_set.to_list paths)
            ~dir:(Abspath.extend temp_dir (Node_hash.to_file_name (Rev.node_hash rev)))
        in
        rev, map)
  in
  let avoid_too_many_open_files =
    Throttle.create ~continue_on_error:false ~max_concurrent_jobs:200
  in
  let paths_in_repo_by_rev =
    List.Assoc.map paths_in_repo_by_rev ~f:(fun map ->
      Map.map map ~f:(fun cached_file ->
        Lazy_deferred.create (fun () ->
          Throttle.enqueue avoid_too_many_open_files (fun () ->
            (if is_testing_client_raises_creating_hunks ()
             then failwithf "exn: %s" (Abspath.to_string cached_file) ());
            Reader.file_contents (Abspath.to_string cached_file)))))
    |> Rev.Compare_by_hash.Table.of_alist_exn
  in
  List.map diff4s ~f:(fun diff4 ->
    try_with_and_ignore_subsequent_errors (fun () ->
      hunks_of_diff4 ~reviewer ~context ~lines_required_to_separate_ddiff_hunks
        paths_in_repo_by_rev diff4))
;;

let print_diff4s ~repo_root ~diff4s ~reviewer ~context ~lines_required_to_separate_ddiff_hunks =
  Path.with_temp_dir (File_name.of_string "fe_cat_") ~f:(fun temp_dir ->
    let%bind files =
      create_files_for_review ~temp_dir ~repo_root ~diff4s ~reviewer
        ~context ~lines_required_to_separate_ddiff_hunks
    in
    let%map hunks = Deferred.List.all files in
    hunks
    |> List.concat_map ~f:ok_exn
    |> Review_main.hunks_to_lines
    |> List.iter ~f:print_endline)
;;

let confirm_review_session_id_exn repo_root
      ~action ~feature_path ~for_ ~(which_session : Which_session.t)
      ~display_ascii ~max_output_columns
  =
  (match which_session with
   | This_session _ -> ()
   | Current_session ->
     if not !Async_interactive.interactive
     then failwith "review_session_id is mandatory when non-interactive");
  let%bind rev_zero =
    match repo_root with
    | Error _ -> return None
    | Ok repo_root ->
      let%map rev = Hg.create_rev_zero repo_root in
      Some rev
  in
  let%bind { Get_review_session.Reaction. status; _ } =
    Get_review_session.rpc_to_server_exn
      { feature_path
      ; rev_zero
      ; for_
      ; ensure_reviewing = false
      ; which_session
      ; lock_session = `No
      }
  in
  match status with
  | `Up_to_date | `Bookmark_update_is_pending ->
    raise_s
      [%sexp
        "reviewer is up to date, no current session",
        { feature_path                  : Feature_path.t
        ; requested_for = (for_         : User_name.t)
        }
      ]
  | `Review_session
      { Get_review_session.Review_session.
        review_session_id
      ; review_session_tip
      ; reviewer_in_session
      ; reviewer_in_feature
      ; diff4s_in_session
      ; may_be_reviewed_by            = _
      ; line_count_to_finish_session
      ; line_count_to_goal
      ; is_locked = _
      ; lines_required_to_separate_ddiff_hunks = _
      } ->
    let diff4s_to_review =
      diff4s_in_session
      |> Array.map ~f:Diff4_to_review.review
      |> Array.to_list
    in
    let confirmed =
      if not !Async_interactive.interactive
      then return true
      else (
        print_introduction_summary_for_review
          ~feature_path ~review_session_tip
          ~reviewer_in_session
          ~warn_reviewer:(Some { reviewer_in_feature
                               ; line_count_to_finish_session
                               ; line_count_to_goal
                               })
          ~diff4s_to_review
          ~display_ascii
          ~max_output_columns;
        Async_interactive.ask_yn ~default:false
          (sprintf !"Really %s this %{Feature_path} session%s?"
             action
             feature_path
             (if not (User_name.equal for_ User_name.unix_login)
              then " for " ^ User_name.to_string for_
              else "")))
    in
    match%map confirmed with
    | true  -> `Id review_session_id
    | false -> `Cancelled
;;

let commit_session_exn repo_root ~feature_path ~for_ ~which_session
      ~display_ascii ~max_output_columns
  =
  match%bind
    confirm_review_session_id_exn repo_root ~action:"commit"
      ~feature_path ~for_ ~which_session
      ~display_ascii ~max_output_columns
  with
  | `Id review_session_id ->
    let%map () =
      Session.Commit.rpc_to_server_exn { feature_path; for_; review_session_id }
    in
    `Committed
  | `Cancelled -> return `Cancelled
;;

(* [by] is a list of filenames (as strings) that provide an order.
   [diffs4] is an array of [Diff4_to_review.t]'s that we want to review.
   Return this array, sorted using the order given by [by].
   - The [by] list is allowed to have elements not in [diffs4]. These are ignored.
   - The [diffs4] array is allowed to have elements not in [by]. These are pushed
   to the end of the answer array.
   - The answer array is stably sorted (which is mostly relevant for items not
   appearing in [by] that get pushed to the end.)
*)

let sort_review_files_using_list diff4s_to_review ~by =
  (* [table] maps filename to index in ~by list. *)
  let bylen = List.length by in
  let table = String.Table.create () in
  List.iteri by ~f:(fun i s -> String.Table.set table ~key:s ~data:i);
  (* Make an (i,d) "alist array" of the diffs, where the key i is d's position in ~by. *)
  let diff_to_keyed d =
    let s = Path_in_repo.to_string (Diff4_to_review.path_in_repo_at_f2 d) in
    let i = Option.value ~default:bylen (String.Table.find table s) in
    (i, d)
  in
  let keyed_diffs = Array.map diff4s_to_review ~f:diff_to_keyed in
  Array.stable_sort ~cmp:(fun (i1, _d1) (i2, _d2) -> Int.compare i1 i2) keyed_diffs;
  Array.map ~f:snd keyed_diffs
;;

let sort_review_files_using_file diff4s_to_review path =
  let%map by = Reader.file_lines (Abspath.to_string path) in
  sort_review_files_using_list diff4s_to_review ~by
;;

let sort_review_files_by_lines diff4s_to_review ~reviewer direction =
  let direction = match direction with
    | `By_increasing_review_lines -> 1
    | `By_decreasing_review_lines -> ~-1
  in
  let cmp a b = direction * Int.compare
                              (Diff4_to_review.num_lines a reviewer)
                              (Diff4_to_review.num_lines b reviewer)
  in
  let diff4s_to_review = Array.copy diff4s_to_review in
  Array.stable_sort diff4s_to_review ~cmp;
  diff4s_to_review
;;

let sort_review_files repo_root_and_kind_or_error diff4s_to_review ~reviewer review_sort =
  match review_sort with
  | `Using_file path -> sort_review_files_using_file diff4s_to_review path
  | (`By_increasing_review_lines | `By_decreasing_review_lines) as review_sort ->
    sort_review_files_by_lines diff4s_to_review ~reviewer review_sort
    |> return
  | `Build_order ->
    let%map files =
      Build_order.sort repo_root_and_kind_or_error (Array.to_list diff4s_to_review)
        Diff4_to_review.path_in_repo_at_f2
    in
    Array.of_list files
;;

let review_or_catch_up
      ~is_catch_up_on_archived_feature
      ~mark_as_reviewed
      ~repo_root
      ~repo_root_kind
      ~remote_repo_path
      ~feature_path
      ~feature_tip
      ~reviewer_in_session
      ~warn_reviewer
      ~review_session_tip
      ~diff4s_to_review
      ~may_modify_local_repo
      ~may_commit_session
      ~maybe_sort_review_files
      ~context
      ~lines_required_to_separate_ddiff_hunks
      ~emacs
      ~display_ascii
      ~max_output_columns
  =
  let module M = struct
    type t = Diff4_to_review.t
    let path_in_repo = Diff4_to_review.path_in_repo_at_f2
    let num_lines_in_diff =
      Some (fun t -> Diff4_to_review.num_lines t reviewer_in_session)
    let reviewed = mark_as_reviewed
    let always_open_file_in_emacs = emacs
    let open_file_in_emacs =
      Some (fun t ->
        Emacs.open_file
          (Abspath.append
             (Repo_root.to_abspath repo_root)
             (Path_in_repo.to_relpath (path_in_repo t))
          ))
    ;;
    let may_commit_session = may_commit_session
  end in
  let%bind () =
    if not may_modify_local_repo
    then return ()
    else
    if is_catch_up_on_archived_feature
    then
      pull_and_update_for_catch_up_on_archived_feature
        ~repo_root ~remote_repo_path
        ~review_session_tip
    else
      pull_and_update
        ~repo_root ~remote_repo_path
        ~feature_path ~feature_tip ~review_session_tip
  in
  let diff4s_to_review =
    let diff4s_to_review = Array.copy diff4s_to_review in
    Array.stable_sort diff4s_to_review
      ~cmp:Diff4_to_review.compare_by_path_in_repo_at_f2_for_review;
    diff4s_to_review
  in
  let%bind diff4s_to_review =
    match maybe_sort_review_files with
    | None             -> return diff4s_to_review
    | Some review_sort ->
      sort_review_files (Ok (repo_root, repo_root_kind)) diff4s_to_review
        ~reviewer:reviewer_in_session review_sort
  in
  let diff4s_to_review = Array.to_list diff4s_to_review in
  if List.is_empty diff4s_to_review then assert false;
  print_introduction_summary_for_review
    ~feature_path
    ~review_session_tip
    ~reviewer_in_session
    ~warn_reviewer
    ~diff4s_to_review
    ~display_ascii
    ~max_output_columns;
  match%bind
    Path.with_temp_dir (File_name.of_string "fe_cat_") ~f:(fun temp_dir ->
      let diff4s_to_review =
        List.filter diff4s_to_review ~f:(fun diff4_to_review ->
          not (Diff4_to_review.is_reviewed diff4_to_review))
      in
      let%bind files =
        create_files_for_review
          ~temp_dir
          ~repo_root
          ~diff4s:(List.map diff4s_to_review ~f:Diff4_to_review.diff4)
          ~reviewer:(`Reviewer reviewer_in_session)
          ~context
          ~lines_required_to_separate_ddiff_hunks
      in
      let files = List.zip_exn diff4s_to_review files in
      Review_main.files (module M) files
    )
  with
  | `Quit ->
    let%bind () =
      if is_testing_client_raises_creating_hunks ()
      then Clock.after (sec 0.5)
      else return ()
    in
    print_endline "Quit";
    Shutdown.exit 0
  | `Reviewed ->
    print_endline "Current session reviewed.";
    return `Reviewed
  | `Commit_session ->
    return `Commit_session
;;

(* In [may_modify_others_(review|catch_up)] below, we avoid making an rpc in the common
   case, but fail as early as possible, before the user does any work that would later be
   thrown away due to missing permission or reason. *)

let may_modify_others_review_exn feature_path ~reason ~whose_review =
  let by = User_name.unix_login in
  let can_short_circuit =
    match whose_review with
    | `User user -> (User_name.equal user by)
    | `All_users | `All_users_but _ -> false
  in
  if can_short_circuit
  || is_some (Sys.getenv
                "IRON_FUNCTIONAL_TESTING_CLIENT_DOES_NOT_CHECK_REVIEW_PERMISSIONS")
  then return ()
  else May_modify_others_review.rpc_to_server_exn { feature_path; whose_review; reason }
;;

let may_modify_others_catch_up_exn ~for_ =
  let by = User_name.unix_login in
  if User_name.equal for_ by
  then return ()
  else May_modify_others_catch_up.rpc_to_server_exn { for_ }
;;

module Review_params = struct

  type t =
    { emacs                                     : bool
    ; for_                                      : User_name.t
    ; display_ascii                             : bool
    ; max_output_columns                        : int
    ; raw                                       : bool
    ; context                                   : int
    ; lines_required_to_separate_ddiff_hunks_override : int option
    ; may_modify_local_repo                     : bool
    ; maybe_sort_review_files                   : Command.Param.Review_sort.t option
    }

  let review_params =
    let open Command.Let_syntax in
    let%map_open () = return ()
    and emacs = emacs
    and for_ = for_
    (* Don't let users pass [-display-ascii] because there's no way to pass it into
       Patdiff4.  But Patdiff4 does respect the iron option, so make the rest of the
       review command obey that. *)
    and display_ascii = return Iron_options.display_ascii_always
    and max_output_columns = max_output_columns
    and raw = no_arg_flag "-raw" ~doc:"show Iron's internal representation of the review"
    and context = context ()
    and lines_required_to_separate_ddiff_hunks_override =
      lines_required_to_separate_ddiff_hunks_override
    and may_modify_local_repo = may_modify_local_repo
    and maybe_sort_review_files = maybe_sort_review_files
    and () = interactive
    in
    fun () ->
      let client_config = Client_config.get () in
      let may_modify_local_repo =
        may_modify_local_repo
        && not (Client_config.Cmd.Review.do_not_modify_local_repo client_config)
      in
      let emacs = emacs || Client_config.Cmd.Review.emacs client_config in
      let maybe_sort_review_files =
        Option.first_some maybe_sort_review_files
          (Option.some_if (Client_config.Cmd.Review.sort_build_order client_config)
             `Build_order)
      in
      { emacs
      ; for_
      ; display_ascii
      ; max_output_columns
      ; raw
      ; context
      ; lines_required_to_separate_ddiff_hunks_override
      ; may_modify_local_repo
      ; maybe_sort_review_files
      }
  ;;
end

let print_catch_up_header_and_description
      { Get_catch_up_session.Catch_up_session.
        feature_path
      ; description
      ; _
      } =
  printf "%s\n" (Cmd_show.header_and_description feature_path ~description)
;;

let print_catch_up_attribute_table
      { Get_catch_up_session.Catch_up_session.
        whole_feature_reviewers
      ; owners
      ; base
      ; tip
      ; is_permanent
      ; is_archived
      ; seconder
      ; _
      } ~display_ascii ~max_output_columns =
  print_string
    (Cmd_show.attribute_table_with_fields ~display_ascii ~max_output_columns
       ~whole_feature_reviewers
       ~owners
       ~base
       ~tip
       ~is_permanent
       ~is_archived
       ~show_is_archived_if_not_archived:true
       ~seconder
       ~next_steps:None
       ());
  printf "\n";
;;

let catch_up_review_loop ~warn_if_no_session ~repo_root ~repo_root_kind ~feature_path
      { Review_params.
        emacs
      ; for_
      ; display_ascii
      ; max_output_columns
      ; raw
      ; context
      ; lines_required_to_separate_ddiff_hunks_override
      ; may_modify_local_repo
      ; maybe_sort_review_files
      }
  =
  Deferred.repeat_until_finished true (fun first_session ->
    let%bind reaction =
      Get_catch_up_session.rpc_to_server_exn { feature_path; for_ }
    in
    if raw then (
      reaction
      |> [%sexp_of: Get_catch_up_session.Reaction.t]
      |> Sexp.to_string_hum
      |> print_endline;
      return (`Finished ()))
    else (
      match reaction with
      | `Up_to_date ->
        if first_session && warn_if_no_session
        then printf "No review to catch up on in %s.\n"
               (Feature_path.to_string feature_path);
        return (`Finished ())
      | `Catch_up_session
          ({ Get_catch_up_session.Catch_up_session.
             catch_up_session_id
           ; catch_up_session_tip
           ; reviewer_in_session
           ; diff4s_to_catch_up
           ; remote_repo_path
           ; feature_path
           ; tip
           ; is_archived
           ; lines_required_to_separate_ddiff_hunks
           ; _
           } as catch_up_session) ->
        let lines_required_to_separate_ddiff_hunks =
          Option.value lines_required_to_separate_ddiff_hunks_override
            ~default:lines_required_to_separate_ddiff_hunks
        in
        let diff4s_to_review =
          diff4s_to_catch_up
          |> Array.of_list
          |> Array.map ~f:Diff4_to_review.catch_up
        in
        let%bind () = may_modify_others_catch_up_exn ~for_ in
        let mark_as_reviewed diff4s_to_review =
          Catch_up_diffs.rpc_to_server_exn
            { for_
            ; feature_path
            ; catch_up_session_id
            ; diff4_in_session_ids =
                List.map diff4s_to_review ~f:Diff4_to_review.id
            }
        in
        print_catch_up_header_and_description catch_up_session;
        print_catch_up_attribute_table catch_up_session
          ~display_ascii ~max_output_columns;
        let%map result =
          review_or_catch_up
            ~is_catch_up_on_archived_feature:(Is_archived.to_bool is_archived)
            ~mark_as_reviewed
            ~repo_root
            ~repo_root_kind
            ~remote_repo_path
            ~feature_path
            ~feature_tip:tip
            ~reviewer_in_session
            ~warn_reviewer:None
            ~review_session_tip:catch_up_session_tip
            ~diff4s_to_review
            ~may_modify_local_repo
            ~may_commit_session:false
            ~maybe_sort_review_files
            ~context
            ~lines_required_to_separate_ddiff_hunks
            ~emacs
            ~display_ascii
            ~max_output_columns
        in
        (match result with
         | `Reviewed -> ()
         | `Commit_session -> assert false);
        (* Maybe by now there is another session to review *)
        `Repeat false
    ))
;;

let review_loop ~repo_root ~repo_root_kind ~feature_path ~create_catch_up_for_me
      ~which_files ~reason
      { Review_params.
        emacs
      ; for_
      ; display_ascii
      ; max_output_columns
      ; raw
      ; context
      ; lines_required_to_separate_ddiff_hunks_override
      ; may_modify_local_repo
      ; maybe_sort_review_files
      }
  =
  let%bind rev_zero = Hg.create_rev_zero repo_root in
  let%bind feature =
    Get_feature.rpc_to_server_exn { feature_path; rev_zero  = Some rev_zero }
  in
  printf "%s\n"
    (Cmd_show.header_and_description feature_path ~description:feature.description);
  print_string (Cmd_show.attribute_table feature
                  ~display_ascii ~max_output_columns
                  ~show_feature_id:false ~show_lock_reasons:false
                  ~show_inheritable_attributes:false
                  ~show_next_steps:false
                  ~show_full_compilation_status:false);
  printf "\n";
  let only_print_session =
    raw || (display_ascii && not !Async_interactive.interactive)
  in
  Deferred.repeat_until_finished true (fun first_session ->
    match%bind
      Get_review_session.rpc_to_server
        { feature_path
        ; rev_zero         = Some rev_zero
        ; for_
        ; ensure_reviewing = true
        ; which_session    = Current_session
        ; lock_session     = if only_print_session then `No else `If_applicable
        }
    with
    | Error e ->
      if first_session
      then Error.raise e
      else return (`Finished ())
    | Ok ({ status; feature_tip; remote_rev_zero = _; remote_repo_path; may_second }
          as reaction) ->
      if only_print_session
      then (
        reaction
        |> [%sexp_of: Get_review_session.Reaction.t]
        |> Sexp.to_string_hum
        |> print_endline;
        return (`Finished ()))
      else (
        let maybe_remind_to_second () =
          if User_name.equal for_ User_name.unix_login && may_second
          then print_endline "Please consider seconding the feature, if it is ready.";
        in
        match status with
        | `Bookmark_update_is_pending ->
          maybe_remind_to_second ();
          if first_session
          then print_endline "Nothing to review, but a bookmark update is pending.";
          return (`Finished ())
        | `Up_to_date ->
          maybe_remind_to_second ();
          if first_session then print_endline "Nothing to review.";
          return (`Finished ())
        | `Review_session
            { Get_review_session.Review_session.
              review_session_id
            ; review_session_tip
            ; reviewer_in_session
            ; reviewer_in_feature
            ; diff4s_in_session
            ; may_be_reviewed_by            = _
            ; line_count_to_finish_session
            ; line_count_to_goal
            ; is_locked = _
            ; lines_required_to_separate_ddiff_hunks
            } ->
          let diff4s_to_review =
            diff4s_in_session
            |> Array.map ~f:Diff4_to_review.review
          in
          let diff4s_to_review =
            match which_files with
            | `All -> Some diff4s_to_review
            | `Files files ->
              let files = Path_in_repo.Set.of_list files in
              let diff4s_to_review = Array.filter diff4s_to_review ~f:(fun d ->
                not (Diff4_to_review.is_reviewed d)
                && Set.mem files (Diff4_to_review.path_in_repo_at_f2 d))
              in
              if Array.is_empty diff4s_to_review
              then (
                maybe_remind_to_second ();
                if first_session then
                  print_endline "Nothing to review in the specified files \
                                 in the current session";
                None)
              else Some diff4s_to_review
          in
          match diff4s_to_review with
          | None -> return (`Finished ())
          | Some diff4s_to_review ->
            let%bind () =
              may_modify_others_review_exn feature_path ~reason:(`This reason)
                ~whose_review:(`User for_)
            in
            let mark_as_reviewed diff4s_to_review =
              Reviewed_diffs.rpc_to_server_exn
                { for_
                ; reason
                ; create_catch_up_for_me
                ; even_if_some_files_are_already_reviewed = false
                ; feature_path
                ; review_session_id
                ; diff4_in_session_ids =
                    List.map diff4s_to_review ~f:Diff4_to_review.id
                }
            in
            let lines_required_to_separate_ddiff_hunks =
              Option.value lines_required_to_separate_ddiff_hunks_override
                ~default:lines_required_to_separate_ddiff_hunks
            in
            let%bind result =
              review_or_catch_up
                ~is_catch_up_on_archived_feature:false
                ~mark_as_reviewed
                ~repo_root
                ~repo_root_kind
                ~remote_repo_path
                ~feature_path
                ~feature_tip
                ~reviewer_in_session
                ~warn_reviewer:
                  (Some { reviewer_in_feature
                        ; line_count_to_finish_session
                        ; line_count_to_goal
                        })
                ~review_session_tip
                ~diff4s_to_review
                ~may_modify_local_repo
                ~may_commit_session:true
                ~maybe_sort_review_files
                ~context
                ~lines_required_to_separate_ddiff_hunks
                ~emacs
                ~display_ascii
                ~max_output_columns
            in
            let%map () =
              match result with
              | `Reviewed -> return ()
              | `Commit_session ->
                match%map
                  commit_session_exn (Ok repo_root) ~feature_path ~for_
                    ~which_session:(This_session review_session_id)
                    ~display_ascii
                    ~max_output_columns
                with
                | `Committed
                | `Cancelled -> ()
            in
            (* Maybe by now there is another session to review *)
            `Repeat false))
;;

let catch_up_review_command =
  Command.async'
    ~summary:"start or continue a catch up session"
    ~readme:(fun () -> "\
This command is deprecated and has been subsumed by [fe review].
")
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = catch_up_feature_path_or_current_bookmark
     and review_params = Review_params.review_params
     in
     fun () ->
       let open! Deferred.Let_syntax in
       if am_functional_testing
       then failwith "This command is deprecated.  \
                      Tests should be updated to use [fe review] instead";
       let feature_path = ok_exn feature_path in
       let review_params = review_params () in
       let%bind repo_root, repo_root_kind =
         Cmd_workspace.repo_for_hg_operations_and_kind_exn feature_path
           ~use:`Share (* If the feature is archived, this defaults to the clone *)
       in
       catch_up_review_loop ~warn_if_no_session:true ~repo_root ~repo_root_kind
         ~feature_path review_params
    )
;;

let command =
  let only_catch_up_review_switch = "-only-catch-up-review" in
  let skip_catch_up_review_switch = "-skip-catch-up-review" in
  Command.async'
    ~summary:"start or continue a review session"
    ~readme:(fun () ->
      concat [ "\
[fe review] can be used to see lines to review in a feature and mark them as reviewed.

When there are both [review] and [catch-up] lines to review, the latter are prioritized
and shown first.  For more control over what to review, see the switches:

  " ; skip_catch_up_review_switch ; "
  " ; only_catch_up_review_switch ; "

Attending someone else's review may be done if authorized, and requires the reviewer
to supply a brief reason.  Example:

  $ fe review -for USER1 -reason 'you are out sick today'

By default, reviewing for someone else skips the catch-up review.  Attending someone
else's catch-up requires admin privileges, and may be done with:

  $ fe review " ; only_catch_up_review_switch ; " -for USER
"])
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and feature_path = active_or_catch_up_feature_path_or_current_bookmark
     and reason = review_reason
     and which_files = which_files
     and create_catch_up_for_me = create_catch_up_for_me
     and review_params = Review_params.review_params
     and only_catch_up_review =
       no_arg_flag only_catch_up_review_switch
         ~doc:" only attend the catch-up review"
     and skip_catch_up_review =
       no_arg_flag skip_catch_up_review_switch
         ~doc:" skip the catch-up part when there are both catch-up and review to do"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let review_params = review_params () in
       let create_catch_up_for_me =
         create_catch_up_for_me ~is_reviewing_for:(`User review_params.for_) |> ok_exn
       in
       let is_acting_for_another_user =
         User_name.(<>) review_params.for_ User_name.unix_login
       in
       let () =
         if only_catch_up_review && create_catch_up_for_me
         then failwithf "At most one of the switches %s and %s may be supplied."
                only_catch_up_review_switch
                Switch.create_catch_up_for_me
                ()
       in
       let () =
         if only_catch_up_review && skip_catch_up_review
         then failwithf "At most one of the switches %s and %s may be supplied."
                only_catch_up_review_switch
                skip_catch_up_review_switch
                ()
       in
       let skip_catch_up_review =
         skip_catch_up_review || (not only_catch_up_review && is_acting_for_another_user)
       in
       let feature_path = ok_exn feature_path in
       let%bind repo_root, repo_root_kind =
         Cmd_workspace.repo_for_hg_operations_and_kind_exn feature_path ~use:`Share
       in
       let%bind () =
         if skip_catch_up_review
         then return ()
         else
           catch_up_review_loop ~warn_if_no_session:only_catch_up_review
             ~repo_root ~repo_root_kind ~feature_path
             review_params
       in
       if only_catch_up_review
       then return ()
       else
         review_loop ~repo_root ~repo_root_kind ~feature_path ~create_catch_up_for_me
           ~which_files ~reason review_params
    )
;;

