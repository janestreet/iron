module Stable = struct
  open Core.Core_stable
  open Import_stable

  module Cr_comments = struct
    module V1 = struct
      type t = Cr_comment.V1.t list Or_error.V1.t
      [@@deriving sexp]
    end
  end
end

open! Core
open! Import

module Persist = struct
  module Cr_comments = struct
    include Persistent.Make
        (struct let version = 1 end)
        (Stable.Cr_comments.V1)
  end
  module Current_session_id = struct
    module V1 = struct
      type t = Session_id.Stable.V1.t option
      [@@deriving sexp]
    end
    include Persistent.Make
        (struct let version = 1 end)
        (V1)
  end
  module Brain_file = struct
    include Persistent.Make
        (struct let version = 3 end)
        (Brain.Stable.V3)
    include Register_read_old_version
        (struct let version = 1 end)
        (Brain.Stable.V2)
  end
  module User_name_file = struct
    include Persistent.Make
        (struct let version = 1 end)
        (User_name.Stable.V1)
  end
end

module Goal_subset = struct
  type t =
    | Entire_goal
    | Entire_goal_but_empty_if_satisfied_by of Review_analysis.t
end

module Register_catch_up = struct
  type t =
    { f : 'a .
            'a Query.t
        -> Review_session.t
        -> Catch_up_session.Id_and_kind.t list
        -> unit Or_error.t
    }
  [@@deriving sexp_of]
end

module Brain_with_goal = struct

  type t =
    { brain : Brain.t
    ; mutable at_review_goal : Review_goal.t option
    }
  [@@deriving fields, sexp_of]

  let diff4s_to_goal_internal t review_goal ~indexed_diff4s ~reviewer =
    (* cheap optimization since we set the brain to the goal sometimes *)
    if match t.at_review_goal with
      | Some at_review_goal -> phys_equal at_review_goal review_goal
      | None -> false
    then []
    else Brain.diff4s_needed_to_extend_brain
           t.brain
           ~reviewer
           ~goal:(Review_goal.diff2s review_goal)
           ~could_use:indexed_diff4s
  ;;

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f _ _ field = f field in
      Fields.Direct.iter t
        ~brain:(check Brain.invariant)
        ~at_review_goal:(check (Option.invariant (fun review_goal ->
          Review_goal.invariant review_goal;
          (* The brain and goal being "the same thing" doesn't actually depend on the
             reviewer, so we can just pick one. *)
          let reviewer = Reviewer.synthetic_whole_feature_reviewer in
          let indexed_diff4s = Indexed_diff4s.create [] in
          let missing_diffs =
            diff4s_to_goal_internal { t with at_review_goal = None }
              review_goal ~reviewer ~indexed_diff4s
          in
          [%test_pred: Diff4s.t] List.is_empty missing_diffs
        )))
    )
  ;;

end

type t =
  { user_name                         : User_name.t
  ; mutable is_whole_feature_follower : bool
  ; mutable is_whole_feature_reviewer : bool
  ; mutable is_using_locked_sessions  : bool
  ; serializer                        : Serializer.t
  (* [crs] is [Error] if we cannot show the CRs due to invalid or pending base facts.
     E.g. if the base isn't CR clean, then we don't know how to count CRs at the tip. *)
  ; mutable crs : Cr_comment.t list Or_error.t
  ; mutable num_crs : int Or_error.t
  ; mutable num_xcrs : int Or_error.t
  (* [review_goal] is what we want the user to know.

     [brain] is what the user knows from prior completed sessions.

     [current_session] is [None] if there is no current session.  If [current_session =
     Some session], then there is a current session, and the session transforms [brain] to
     to knowledge that we'll call [session_end].

     [from_brain_to_goal] and [from_session_end_to_goal] are diffs that extend knowledge
     to [review_goal].  They are [Error] iff [review_goal] is [Error].

     Here's a graph, where nodes are [Diff2s.t], and an edge is a [Diff4s.t] that
     extends knowledge from its tail to its head.

     {v
       -------------------------from_brain_to_goal-------------------------
       |                                                                  v
       brain ---session---> session_end ---from_session_end_to_goal---> review_goal
     v}
  *)
  ; mutable review_goal               : Review_goal.t Or_error.t Or_pending.t
  ; mutable indexed_diff4s            : Indexed_diff4s.t Or_error.t Or_pending.t
  ; mutable brain                     : Brain_with_goal.t
  ; mutable current_session           : Review_session.t option
  ; register_catch_up                 : Register_catch_up.t
  ; feature_cache_invalidator         : Cached.Invalidator.t
  ; dynamic_upgrade_state             : Dynamic_upgrade.State.t
  }
[@@deriving fields, sexp_of]

let reviewer t =
  Reviewer.create t.user_name
    ~is_whole_feature_follower:t.is_whole_feature_follower
    ~is_whole_feature_reviewer:t.is_whole_feature_reviewer
;;

let brain t = t.brain.brain

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    (match t.review_goal, t.indexed_diff4s with
     | Known (Ok review_goal), Known (Ok could_use) ->
       Brain.check_diff4s_needed_to_extend_brain_exn
         t.brain.brain
         ~reviewer:(reviewer t)
         ~goal:(Review_goal.diff2s review_goal)
         ~could_use
     | _ -> ());
    Fields.iter
      ~user_name:(check User_name.invariant)
      ~is_whole_feature_follower:ignore
      ~is_whole_feature_reviewer:ignore
      ~is_using_locked_sessions:ignore
      ~crs:(check (Or_error.invariant (fun crs ->
        List.iter crs ~f:(fun cr ->
          match Cr_comment.assignee cr with
          | Missing_file_owner -> assert false
          | Feature_owner ->
            ()
          (* The invariant that the assignee must always be the same as the review manager
             does not hold anymore because the CR can be assigned to an alias of the
             review manager's user (at the time of update_crs) instead. *)
          | This unresolved_name -> Unresolved_name.invariant unresolved_name))))
      ~num_crs:(check (Or_error.invariant
                         ([%test_pred: int] (fun num_crs -> num_crs >= 0))))
      ~num_xcrs:(check (Or_error.invariant
                          ([%test_pred: int] (fun num_crs -> num_crs >= 0))))
      ~current_session:(check (Option.iter ~f:(fun review_session ->
        Review_session.invariant review_session;
        (* Once all diffs are reviewed in a review session, we incorporate it into
           [brain], and clear the current session.  This also implies that the review
           session isn't empty, i.e. has at least one diff4_in_session. *)
        assert (not (Review_session.all_diff4s_are_reviewed review_session)))))
      ~brain:(check Brain_with_goal.invariant)
      ~review_goal:
        (check (Or_pending.invariant (Or_error.invariant Review_goal.invariant)))
      ~indexed_diff4s:
        (check (Or_pending.invariant (Or_error.invariant Indexed_diff4s.invariant)))
      ~register_catch_up:ignore
      ~feature_cache_invalidator:(check Cached.Invalidator.invariant)
      ~dynamic_upgrade_state:(check Dynamic_upgrade.State.Reference.invariant)
      ~serializer:(check Serializer.invariant))
;;

let crs_file = Relpath.of_string "crs"
let user_name_file = Relpath.of_string "user-name"
let brain_file = Relpath.of_string "known"
let current_session_id_file = Relpath.of_string "current-session-id"

let review_session_dir review_session_id =
  Relpath.of_list
    [ File_name.of_string "review-sessions"
    ; Session_id.to_file_name review_session_id
    ]
;;

let persist_brain t =
  Serializer.set_contents t.serializer t.brain.brain
    ~file:brain_file (module Persist.Brain_file)
;;

let persist_crs t =
  Serializer.set_contents t.serializer
    ~file:crs_file t.crs (module Persist.Cr_comments);
;;

let persist_current_session_id t =
  Serializer.set_contents t.serializer
    (Option.map t.current_session ~f:Review_session.id)
    ~file:current_session_id_file
    (module Persist.Current_session_id)
;;

let session_is_in_progress review_session =
  Review_session.is_locked review_session
  || Review_session.have_done_some_review review_session
;;

let have_session_in_progress t =
  match t.current_session with
  | None                -> false
  | Some review_session -> session_is_in_progress review_session
;;

let have_done_some_review_in_current_session t =
  match t.current_session with
  | None                -> false
  | Some review_session -> Review_session.have_done_some_review review_session
;;

let diff4s_from_brain_to_goal t ~(brain : Brain_with_goal.t) =
  match t.review_goal, t.indexed_diff4s with
  | (Pending_since _ | Known (Error _ ) as z), _
  | _, (Pending_since _ | Known (Error _) as z)
    -> z
  | Known (Ok review_goal), Known (Ok indexed_diff4s) ->
    let diff4s =
      Brain_with_goal.diff4s_to_goal_internal brain review_goal
        ~indexed_diff4s ~reviewer:(reviewer t)
    in
    Known (Ok (diff4s, review_goal))
;;

let brain_if_session_finished t session =
  { Brain_with_goal.
    brain =
      Brain.extend t.brain.brain
        ~with_:(Review_session.diff4s session)
        ~reviewer:(reviewer t)
        ~mark_kind:User
  ; at_review_goal = None
  }
;;

let brain_if_session_was_committed t session =
  { Brain_with_goal.
    brain =
      Brain.extend t.brain.brain
        ~with_:(Review_session.reviewed_diff4s session)
        ~reviewer:(reviewer t)
        ~mark_kind:User
  ; at_review_goal = None
  }
;;

let set_current_session t x =
  t.current_session <- x;
  persist_current_session_id t;
;;

let clear_current_session t =
  if is_some t.current_session then set_current_session t None;
;;

let set_brain t brain =
  assert (not (have_done_some_review_in_current_session t));
  let brain =
    match diff4s_from_brain_to_goal t ~brain with
    | Pending_since _ | Known (Error _) -> brain
    | Known (Ok (diff4s, review_goal)) ->
      match diff4s with
      | [] ->
        if phys_equal t.brain brain
        then (
          (* We set [t.brain] to the goal so that the [phys_equal] optimization in
             [diff4s_to_goal] will apply in the future.  And we return [t.brain] so that
             the [phys_equal] test below applies, and we don't persist an identical
             version of the brain.  This avoids re-persisting many brains on startup,
             because even in cases where [brain] is at [review_goal], their
             deserialization won't be [phys_equal]. *)
          t.brain.at_review_goal <- Some review_goal;
          t.brain)
        else { Brain_with_goal. brain = brain.brain; at_review_goal = Some review_goal }
      | _::_ ->
        let reviewer = reviewer t in
        let all_diff4s_are_implicitly_reviewed = ref true in
        let implicitly_reviewed_diff4s =
          List.filter diff4s ~f:(fun { diff4; _ } ->
            let is_implicitly_reviewed = Diff4.is_implicitly_reviewed diff4 reviewer in
            all_diff4s_are_implicitly_reviewed :=
              is_implicitly_reviewed && !all_diff4s_are_implicitly_reviewed;
            is_implicitly_reviewed)
        in
        let all_diff4s_are_implicitly_reviewed = !all_diff4s_are_implicitly_reviewed in
        let has_implicit_actual_diffs =
          (* The purpose of rev updates is to reduce the amount of revisions in brains, by
             trying to advance many brains to the tip of the feature. But advancing parts
             of brains using rev updates tends to make different files have different
             revisions, so we can have lots of revisions. So we only use the rev_updates
             when we're not going to introduce more revisions, ie when we have
             non-rev-updates implicit diffs to stick in the brain anyway, or when we are
             going to advance the whole brain. *)
          List.exists implicitly_reviewed_diff4s
            ~f:(fun d -> not (Diff4.is_rev_update d.diff4))
        in
        let should_extend_brain_implicitly =
          not (List.is_empty implicitly_reviewed_diff4s)
          && (all_diff4s_are_implicitly_reviewed || has_implicit_actual_diffs)
        in
        if not should_extend_brain_implicitly
        then brain
        else (
          let at_review_goal =
            if all_diff4s_are_implicitly_reviewed
            then Some review_goal
            else None
          in
          let brain =
            Brain.extend brain.brain
              ~with_:implicitly_reviewed_diff4s ~reviewer ~mark_kind:User
          in
          { Brain_with_goal.brain; at_review_goal })
  in
  (* This check avoids deleting empty sessions needlessly when calling [set_brain t
     t.brain] *)
  if not (phys_equal brain t.brain);
  then (
    t.brain <- brain;
    persist_brain t;
    clear_current_session t)
;;

let check_session_id t session_id =
  match t.current_session with
  | None -> error_string "there is no current session"
  | Some session ->
    Session_id.check ~actual:(Review_session.id session) ~supplied:session_id
;;

let commit_current_session_internal t =
  match t.current_session with
  | None -> ()
  | Some session ->
    let reviewed_diff4s = Review_session.reviewed_diff4s session in
    let brain =
      Brain.extend t.brain.brain
        ~with_:reviewed_diff4s ~reviewer:(reviewer t) ~mark_kind:User
    in
    clear_current_session t;
    set_brain t { brain; at_review_goal = None }
;;

let set_crs t crs ~persist =
  (* This avoids rewriting crs files needlessly (7500 -> 300 in my test on startup), both
     when repartitioning and also when receiving an update bookmark.  For the first time,
     [crs <> t.crs] because t.crs is [undefined] so we do persist. *)
  match
    [%compare: Cr_comment.Structurally_compared.t list Or_error.t] crs t.crs
  with
  | 0 -> ()
  | _ ->
    t.crs <- crs;
    if persist then persist_crs t;
    match t.crs with
    | Error _ as e -> t.num_crs <- e; t.num_xcrs <- e
    | Ok crs ->
      let num_crs = ref 0 in
      let num_xcrs = ref 0 in
      List.iter crs ~f:(fun cr ->
        if Cr_comment.is_xcr cr then incr num_xcrs else incr num_crs);
      t.num_crs <- Ok !num_crs;
      t.num_xcrs <- Ok !num_xcrs;
;;

let update_crs t crs ~(base_facts : Rev_facts.t Or_pending.t) =
  let crs =
    if List.is_empty crs
    then Ok []
    else (
      match base_facts with
      | Pending_since _ -> error_string "it is not known whether base is CR clean"
      | Known base_facts ->
        match
          Rev_facts.Is_cr_clean.check base_facts.is_cr_clean
            (Rev_facts.rev base_facts)
        with
        | Error e -> error "Iron bug -- please report" e [%sexp_of: Error.t]
        | Ok base_is_cr_clean ->
          if base_is_cr_clean
          then Ok crs
          else error_string "base is not CR clean")
  in
  set_crs t crs ~persist:true;
;;

let undefined = error_string "undefined -- please report Iron bug"

let create user_name ~is_whole_feature_follower ~is_whole_feature_reviewer
      ~is_using_locked_sessions
      ~cr_comments ~base_facts
      ~register_catch_up ~feature_cache_invalidator
      ~dynamic_upgrade_state
      serializer =
  Serializer.add_cache_invalidator serializer feature_cache_invalidator;
  Serializer.set_contents serializer
    ~file:user_name_file user_name (module Persist.User_name_file);
  let pending = pending_now () in
  let t =
    { user_name
    ; crs = undefined
    ; num_crs = undefined
    ; num_xcrs = undefined
    ; is_whole_feature_follower
    ; is_whole_feature_reviewer
    ; is_using_locked_sessions
    ; review_goal = pending
    ; indexed_diff4s = pending
    ; brain = { brain = Brain.empty; at_review_goal = None }
    ; current_session = None
    ; register_catch_up
    ; feature_cache_invalidator
    ; dynamic_upgrade_state
    ; serializer
    }
  in
  update_crs t cr_comments ~base_facts;
  persist_brain t;
  persist_current_session_id t;
  t
;;

let what_to_do_with_session t ~(which_session : Which_session.t) =
  match t.current_session with
  | None ->
    (match which_session with
     | Current_session -> `No_current_session
     | This_session supplied -> Error.raise (Session_id.no_session_error ~supplied))
  | Some session ->
    let must_use_it =
      match which_session with
      | Current_session -> false
      | This_session supplied ->
        ok_exn (Session_id.check ~actual:(Review_session.id session) ~supplied);
        true
    in
    match t.review_goal with
    | Pending_since _ | Known (Error _) -> `Use_it session
    | Known (Ok goal) ->
      (* We prefer people to have the feature goal as their session goal.  So, we only use
         a session if review has been done or if its goal is the review goal. *)
      if must_use_it
      || session_is_in_progress session
      || (Rev.equal_node_hash
            (Review_session.tip session)
            (Review_goal.tip_rev goal)
          && Rev.equal_node_hash
               (Review_session.base session)
               (Review_goal.base_rev goal)
          && Reviewer.equal (reviewer t) (Review_session.reviewer session))
      then `Use_it session
      else `Drop_it
;;

let forget_from_brain_internal_exn t ~what_to_forget =
  (match t.current_session with
   | None -> ()
   | Some session ->
     (* Forgetting when the session is locked is authorized, as long as no files are
        reviewed. *)
     if Review_session.have_done_some_review session
     then failwith "cannot forget -- some files are marked as reviewed in the current \
                    review session");
  let brain =
    match what_to_forget with
    | `All -> []
    | `Files paths_in_repo ->
      let files_to_forget =
        (* This silently allows dupes in [paths_in_repo], which seems OK. *)
        let table = Path_in_repo.Table.create () in
        List.iter paths_in_repo ~f:(fun path_in_repo ->
          Hashtbl.set table ~key:path_in_repo ~data:(ref false));
        table
      in
      let brain =
        List.filter t.brain.brain ~f:(fun { diff2; _ } ->
          match Hashtbl.find files_to_forget diff2.tip.path_in_repo with
          | None -> true
          | Some used -> used := true; false)
      in
      let unused_files_to_forget =
        List.filter_map (Hashtbl.to_alist files_to_forget)
          ~f:(fun (path_in_repo, used) -> if !used then None else Some path_in_repo)
      in
      if not (List.is_empty unused_files_to_forget)
      then raise_s [%sexp "there is no diff in your brain for"
                        , (unused_files_to_forget : Path_in_repo.t list)];
      brain
  in
  set_brain t { brain; at_review_goal = None }
;;

let create_session_from_brain_to_goal_exn t =
  assert (is_none t.current_session);
  (* Calling [set_brain] shouldn't be necessary, because we try to make the
     brain match the goal as soon as possible; but in case we're missing a case, this
     makes the code more robust. *)
  set_brain t t.brain;
  let from_brain_to_goal, review_goal =
    match known_exn (diff4s_from_brain_to_goal t ~brain:t.brain) with
    | Ok x -> x
    | Error _ ->
      failwith "\
cannot create review session -- the feature has problems that need to be fixed"
  in
  if List.is_empty from_brain_to_goal
  then `Up_to_date
  else (
    let session =
      Review_session.create
        ~reviewer:(reviewer t)
        ~serializer_dir_of_id:review_session_dir
        ~diff4s:from_brain_to_goal
        ~tip:(Review_goal.tip_rev review_goal)
        ~base:(Review_goal.base_rev review_goal)
        ~feature_cache_invalidator:t.feature_cache_invalidator
        ~dynamic_upgrade_state:t.dynamic_upgrade_state
        t.serializer
    in
    set_current_session t (Some session);
    `Review_session session)
;;

let register_catch_up t query review_session = function
  | [] -> Ok ()
  | (_::_) as ids_and_kinds ->
    t.register_catch_up.f query review_session ids_and_kinds
;;

let num_lines_completed t =
  let num_lines_completed_in_brain =
    let reviewer = reviewer t in
    List.sum (module Int) t.brain.brain ~f:(fun { diff2; _ } ->
      Diff4.num_lines (Diff4.create_from_scratch_to_diff2 diff2) reviewer)
  in
  let num_lines_completed_in_session =
    match what_to_do_with_session t ~which_session:Current_session with
    | `No_current_session | `Drop_it -> 0
    | `Use_it session -> Review_session.num_lines_completed session
  in
  num_lines_completed_in_brain + num_lines_completed_in_session
;;

let have_done_some_review t = num_lines_completed t <> 0

let reviewed_diff4s_output_in_current_session t =
  match t.current_session with
  | None -> []
  | Some review_session ->
    List.map (Review_session.reviewed_diff4s review_session)
      ~f:Diff4.And_output_num_lines.output
;;

let have_potentially_blocking_review_session_in_progress t =
  have_session_in_progress t
  && (let reviewer = reviewer t in
      let is_potentially_blocking diff2 =
        Diff2.may_review diff2 ~include_may_follow:false reviewer
      in
      List.exists t.brain.brain ~f:(fun { diff2; mark_kind = _ } ->
        is_potentially_blocking diff2)
      || List.exists (reviewed_diff4s_output_in_current_session t)
           ~f:is_potentially_blocking)
;;

let would_generate_catch_up_upon_release_if_not_reviewed t brain =
  let what_would_be_extended = Brain.what_would_be_extended brain in
  let reviewer = reviewer t in
  Staged.stage (fun (diff4 : Diff4.t) ->
    let is_follower_of_that_diff4 () =
      match Diff4.may_follow diff4 reviewer with
      | `Dropped_from_follow | `Follow_lines -> true
      | `Nothing_to_follow -> false
    in
    let is_partially_known () =
      match ok_exn ((force what_would_be_extended) diff4) with
      | `New (_ : Diff2.t) -> false
      | `Extends { diff2 = _; mark_kind } ->
        match mark_kind with
        | Internal__fully_reviewed -> false
        | User -> true
    in
    if is_follower_of_that_diff4 ()
    then Some Catch_up_kind.Follower
    else if is_partially_known ()
    then Some Catch_up_kind.Unfinished_review
    else None
  )
;;

module Review_lines_of_diff4 = struct
  type t =
    { diff4       : Diff4.t
    ; lines       : int
    ; review_kind : Review_kind.t
    }
  [@@deriving sexp_of]

  let sum_review_lines list =
    List.fold list ~init:Line_count.Review.zero ~f:(fun review_lines t ->
      Line_count.Review.add_count review_lines t.review_kind t.lines)
  ;;
end

module Line_count_to_goal = Iron_protocol.Get_review_session.Line_count_to_goal

module Review_lines_to_goal_via_session = struct
  type 'a t =
    { to_finish_session  : 'a
    ; line_count_to_goal : 'a Or_error.t Or_pending.t Line_count_to_goal.t
    }
  [@@deriving fields, sexp_of]

  let map t ~f =
    let map f field = Field.get field t |> f in
    Fields.map
      ~to_finish_session:(map f)
      ~line_count_to_goal:
        (map (Line_count_to_goal.map ~f:(Or_pending.map ~f:(Or_error.map ~f))))
  ;;

  let review_lines ({ to_finish_session
                    ; line_count_to_goal =
                        { from_session_end
                        ; from_brain_if_session_was_committed = _
                        }
                    } : Review_lines_of_diff4.t list t) =
    let to_finish_session =
      to_finish_session
      |> Review_lines_of_diff4.sum_review_lines
    in
    match Or_pending.or_pending_error from_session_end with
    | Ok from_session_end ->
      To_goal_via_session.Fully_known
        (Line_count.Review.(+)
           to_finish_session
           (Review_lines_of_diff4.sum_review_lines from_session_end))
    | Error from_session_end_to_goal ->
      To_goal_via_session.Partially_known
        { to_finish_session; from_session_end_to_goal }
  ;;
end

let compute_review_lines_of_diff4 t brain =
  let would_generate_catch_up_upon_release =
    Staged.unstage (would_generate_catch_up_upon_release_if_not_reviewed t brain)
  in
  let f (goal_subset : Goal_subset.t)
        ({ diff4; output_num_lines } : Diff4.And_output_num_lines.t) ~reviewer =
    let lines = Diff4.num_lines diff4 reviewer in
    let review_kind : Review_kind.t =
      match Diff4.may_review diff4 reviewer with
      | `Dropped_from_follow | `Follow_lines -> Follow
      | `Nothing_to_review_or_follow
      | `Dropped_from_review
      | `Review_ownership_change
      | `Review_lines
        ->
        if match goal_subset with
          | Entire_goal -> false
          | Entire_goal_but_empty_if_satisfied_by review_analysis ->
            not reviewer.is_whole_feature_reviewer
            && (Diff4.is_forget diff4
                || Review_analysis.non_wfr_obligations_will_be_satisfied_once_expected_users_have_read
                     review_analysis
                     (Diff4.output diff4 ~num_lines_in_diff:output_num_lines))
        then
          if Option.is_some (would_generate_catch_up_upon_release diff4)
          then Follow
          else if Diff4.should_review_ownership_change diff4 reviewer
          then Ownership_change
          else May_review
        else Must_review
    in
    { Review_lines_of_diff4. diff4; lines; review_kind }
  in
  Staged.stage f
;;

let review_lines_to_goal_via_session t (goal_subset : Goal_subset.t) =
  let to_finish_session
    , brain_if_session_was_finished
    , brain_if_session_was_committed
    =
    match what_to_do_with_session t ~which_session:Current_session with
    | `No_current_session | `Drop_it -> [], t.brain, t.brain
    | `Use_it session ->
      let to_finish_session =
        let compute_lines =
          Staged.unstage (compute_review_lines_of_diff4 t t.brain.brain)
        in
        Review_session.not_reviewed_diff4s session
        |> List.map ~f:(fun diff4 ->
          compute_lines goal_subset diff4 ~reviewer:(Review_session.reviewer session))
      in
      to_finish_session
    , brain_if_session_finished      t session
    , brain_if_session_was_committed t session
  in
  let reviewer = reviewer t in
  let from_brain_to_goal (brain : Brain_with_goal.t) =
    let compute_lines =
      Staged.unstage (compute_review_lines_of_diff4 t brain.brain)
    in
    diff4s_from_brain_to_goal t ~brain
    |> Or_pending.Or_error.map ~f:(fun (from_brain_to_goal, _) ->
      List.map from_brain_to_goal ~f:(fun diff4 ->
        compute_lines goal_subset diff4 ~reviewer))
  in
  let from_session_end = from_brain_to_goal brain_if_session_was_finished in
  let from_brain_if_session_was_committed =
    if phys_equal brain_if_session_was_finished brain_if_session_was_committed
    then from_session_end
    else from_brain_to_goal brain_if_session_was_committed
  in
  { Review_lines_to_goal_via_session.
    to_finish_session
  ; line_count_to_goal = { from_session_end; from_brain_if_session_was_committed }
  }
;;

let line_count_remaining_to_review t goal_subset =
  Review_lines_to_goal_via_session.review_lines
    (review_lines_to_goal_via_session t goal_subset)
;;

let line_count_cached_in_feature t goal_subset =
  { Line_count.Cached_in_feature.
    review = line_count_remaining_to_review t goal_subset
  ; completed = num_lines_completed t
  ; have_potentially_blocking_review_session_in_progress
    = have_potentially_blocking_review_session_in_progress t
  }
;;

let dump_review_lines t goal_subset =
  let detail = review_lines_to_goal_via_session t goal_subset in
  let total = Review_lines_to_goal_via_session.review_lines detail in
  let user = t.user_name in
  [%sexp
    { user   : User_name.t
    ; total  : Line_count.Review.t To_goal_via_session.t
    ; detail : Review_lines_of_diff4.t list Review_lines_to_goal_via_session.t
    }
  ]
;;

let can_make_progress t goal_subset =
  let review =
    line_count_remaining_to_review t goal_subset
    |> To_goal_via_session.maybe_partially_known
    |> Line_count.Review.to_review_column_shown
         ~have_potentially_blocking_review_session_in_progress:
           (have_potentially_blocking_review_session_in_progress t)
  in
  Review_or_commit.count review > 0
;;

let compute_review_kind t session goal_subset =
  let reviewer = Review_session.reviewer session in
  let compute_lines = Staged.unstage (compute_review_lines_of_diff4 t t.brain.brain) in
  Staged.stage (fun diff4 ->
    let review_lines_of_diff4 =
      compute_lines goal_subset ~reviewer { diff4 ; output_num_lines = 1 }
    in
    review_lines_of_diff4.review_kind
  )
;;

let review_session_to_protocol t session goal_subset
      ~may_be_reviewed_by
      ~lines_required_to_separate_ddiff_hunks
  : Iron_protocol.Get_review_session.Review_session.t =
  let reviewer_in_session = Review_session.reviewer session in
  let diff4s_in_session =
    Review_session.diff4s_and_review_kind_in_session_not_implicitly_reviewed session
      ~compute_review_kind:(Staged.unstage (compute_review_kind t session goal_subset))
  in
  let review_lines_to_goal_via_session =
    review_lines_to_goal_via_session t goal_subset
    |> Review_lines_to_goal_via_session.map ~f:Review_lines_of_diff4.sum_review_lines
  in
  { review_session_id            = Review_session.id  session
  ; review_session_tip           = Review_session.tip session
  ; reviewer_in_session
  ; reviewer_in_feature          = reviewer t
  ; diff4s_in_session
  ; may_be_reviewed_by
  ; line_count_to_finish_session = review_lines_to_goal_via_session.to_finish_session
  ; line_count_to_goal           = review_lines_to_goal_via_session.line_count_to_goal
  ; is_locked                    = Review_session.is_locked session
  ; lines_required_to_separate_ddiff_hunks
  }
;;

let get_session_internal_exn t ~which_session =
  match what_to_do_with_session t ~which_session with
  | `Use_it session     -> `Review_session session
  | `No_current_session -> create_session_from_brain_to_goal_exn t
  | `Drop_it ->
    clear_current_session t;
    create_session_from_brain_to_goal_exn t
;;

let get_session_exn t goal_subset
      ~may_be_reviewed_by ~lines_required_to_separate_ddiff_hunks
      ~which_session =
  match get_session_internal_exn t ~which_session with
  | `Up_to_date as result -> result
  | `Review_session session ->
    `Review_session
      (review_session_to_protocol t session goal_subset
         ~may_be_reviewed_by ~lines_required_to_separate_ddiff_hunks)
;;

let maybe_advance_brain t =
  match what_to_do_with_session t ~which_session:Current_session with
  | `No_current_session | `Drop_it -> set_brain t t.brain
  | `Use_it session ->
    if Review_session.all_diff4s_are_reviewed session
    then commit_current_session_internal t
    else if not (session_is_in_progress session)
    then set_brain t t.brain
;;

let check_cr_clean t =
  try
    if ok_exn t.num_crs  > 0 then failwith "CRs remain" ;
    if ok_exn t.num_xcrs > 0 then failwith "XCRs remain";
    Ok ()
  with exn ->
    error "not CR clean" (t.user_name, exn) [%sexp_of: User_name.t * exn]
;;

module Review_authorization = struct
  module Reviewed_by_themselves = struct
    type t = { create_catch_up_for_me : bool }
    [@@deriving sexp_of]
  end

  module Reviewed_by_someone_else = struct
    type t =
      { catch_up_kind                   : Catch_up_kind.Reviewed_by_someone_else.t
      ; for_user_with_only_follow_lines : bool
      ; reviewed_for                    : User_name.t
      }
    [@@deriving sexp_of]
  end

  type t =
    | Reviewed_by_someone_else of Reviewed_by_someone_else.t
    | Reviewed_by_themselves   of Reviewed_by_themselves.t
    | Reviewed__no_catch_up_as_per_user_info
  [@@deriving sexp_of]

  let should_register_catch_up : t -> Catch_up_kind.t option = function
    | Reviewed__no_catch_up_as_per_user_info -> None
    | Reviewed_by_someone_else t -> Some (Reviewed_by_someone_else t.catch_up_kind)
    | Reviewed_by_themselves t ->
      match t with
      | { create_catch_up_for_me = true  } -> Some Create_catch_up_for_me
      | { create_catch_up_for_me = false } -> None
  ;;

  let create
        review_manager
        ~allow_review_for
        ~are_acting_for_themselves_or_for_invalid_user
        ~current_feature_goal_subset
        ~reason
        ~create_catch_up_for_me
        query =
    Or_error.try_with (fun () ->
      let reviewed_by = Query.by query in
      let reviewed_for = review_manager.user_name in
      let is_self = User_name.equal reviewed_for reviewed_by in
      if create_catch_up_for_me && not is_self
      then
        raise_s
          [%sexp
            (sprintf "cannot use switch [%s] when reviewing for someone else"
               Switch.create_catch_up_for_me : string),
            { requested_by  = (reviewed_by  : User_name.t)
            ; reviewing_for = (reviewed_for : User_name.t)
            }
          ];
      if is_self
      then Reviewed_by_themselves { create_catch_up_for_me }
      else if are_acting_for_themselves_or_for_invalid_user
                ~for_:reviewed_for ~by:reviewed_by
      then Reviewed__no_catch_up_as_per_user_info
      else (
        let for_user_with_only_follow_lines =
          match
            line_count_remaining_to_review review_manager current_feature_goal_subset
          with
          | Partially_known _ -> false
          | Fully_known review_lines ->
            let review =
              Line_count.Review.to_review_column_shown review_lines
                ~have_potentially_blocking_review_session_in_progress:
                  (have_potentially_blocking_review_session_in_progress review_manager)
            in
            Review_or_commit.count review = 0 && review_lines.follow > 0
        in
        ok_exn (Allow_review_for.check allow_review_for ~reviewed_for ~reviewed_by);
        let reason =
          match reason with
          | `Not_supported -> ""
          | `This "" -> failwith "must supply -reason when acting for someone"
          | `This reason -> reason
        in
        Reviewed_by_someone_else
          { catch_up_kind = { reviewed_by; reason }
          ; for_user_with_only_follow_lines
          ; reviewed_for
          }))
  ;;

  let unauthorized_for_user_with_only_follow_lines = function
    | Reviewed_by_someone_else t ->
      if t.for_user_with_only_follow_lines
      then
        Or_error.error "unauthorized review for a user with only lines to follow"
          t.reviewed_for [%sexp_of: User_name.t]
      else Ok ()
    | Reviewed_by_themselves _
    | Reviewed__no_catch_up_as_per_user_info -> Ok ()
  ;;

  let requires_witness (_ : t) = ()
end

let commit_current_session_exn t review_authorization session_id =
  match t.current_session with
  | None -> ()
  | Some _ ->
    ok_exn (check_session_id t session_id);
    Review_authorization.requires_witness review_authorization;
    commit_current_session_internal t
;;

let set_session_is_locked_exn t review_authorization query ~which_session is_locked =
  match t.current_session with
  | None -> failwith "user has no session"
  | Some session ->
    (match (which_session : Which_session.t) with
     | Current_session -> ()
     | This_session supplied_id -> ok_exn (check_session_id t supplied_id));
    Review_authorization.requires_witness review_authorization;
    Review_session.set_is_locked session query is_locked;
;;

let forget_from_brain_exn t review_authorization ~what_to_forget =
  Review_authorization.requires_witness review_authorization;
  forget_from_brain_internal_exn t ~what_to_forget
;;

let reviewed t query review_session_id diff4_in_session_ids
      goal_subset review_authorization ~even_if_some_files_are_already_reviewed =
  match t.current_session with
  | None -> error_string "there is no current session"
  | Some session ->
    Review_authorization.requires_witness review_authorization;
    let compute_review_kind =
      Staged.unstage (compute_review_kind t session goal_subset)
    in
    match
      Review_session.reviewed session query review_session_id diff4_in_session_ids
        ~compute_review_kind ~is_using_locked_sessions:t.is_using_locked_sessions
        ~even_if_some_files_are_already_reviewed
    with
    | Error _ as error -> error
    | Ok () ->
      let register_catch_up =
        match Review_authorization.should_register_catch_up review_authorization with
        | None -> Ok ()
        | Some kind ->
          register_catch_up t query session
            (List.map diff4_in_session_ids ~f:(fun id ->
               { Catch_up_session.Id_and_kind. id; kind }))
      in
      maybe_advance_brain t;
      register_catch_up
;;

let forget_from_current_session_exn t review_authorization query session_id ~what_to_forget =
  match t.current_session with
  | None -> failwith "there is no current session"
  | Some current_session ->
    Review_authorization.requires_witness review_authorization;
    (match what_to_forget with
     | `All ->
       ok_exn (Review_session.set_to_nothing_reviewed current_session query session_id);
     | `Files paths_in_repo ->
       let ids_by_path =
         current_session
         |> Review_session.diff4s_in_session_not_implicitly_reviewed
         |> Array.map ~f:(fun diff ->
           Review_session.Diff4_in_session.path_in_repo_at_f2 diff,
           Review_session.Diff4_in_session.id diff)
         |> Array.to_list
         |> Path_in_repo.Table.of_alist_multi
       in
       let diff4_in_session_ids =
         paths_in_repo
         |> List.filter_map ~f:(Hashtbl.find ids_by_path)
         |> List.concat
       in
       ok_exn (Review_session.unreviewed
                 current_session query session_id diff4_in_session_ids));
    maybe_advance_brain t;
;;

let update_review_goal t ~review_goal ~indexed_diff4s
      ~is_whole_feature_follower
      ~is_whole_feature_reviewer =
  t.is_whole_feature_follower <- is_whole_feature_follower;
  t.is_whole_feature_reviewer <- is_whole_feature_reviewer;
  let review_goal =
    Or_error.map review_goal ~f:(fun review_goal ->
      Review_goal.restrict_to_may_review_or_follow review_goal (reviewer t))
  in
  t.review_goal    <- Known review_goal;
  t.indexed_diff4s <- Known indexed_diff4s;
  maybe_advance_brain t;
;;

let commit_session_then_create_catch_up_to_goal_exn__allow_all t query ~reason =
  (* If we don't have a review goal, let's fail before doing any side effect. *)
  ignore (ok_known_exn t.review_goal : Review_goal.t);
  match reason with
  | `Internal_mark__no_catch_up_allow_for_all -> ()
  | `Release
  | `Review_authorization _ as reason ->
    (* We [commit_current_session] to create a session with just the unreviewed diffs,
       so we can then create a catch-up session with just those diffs.  We know that
       the call to [register_catch_up] below will return [Ok ()], because we are
       supplying exactly the ids in the session. *)
    commit_current_session_internal t;
    (* [create_session_from_brain_to_goal_exn] won't raise, because [t.review_goal]
       is [Known (Ok _)]. *)
    match create_session_from_brain_to_goal_exn t with
    | `Up_to_date -> ()
    | `Review_session session ->
      let ids =
        match reason with
        | `Review_authorization review_authorization ->
          (match Review_authorization.should_register_catch_up review_authorization with
           | None -> []
           | Some kind ->
             Review_session.diff4s_in_session_not_implicitly_reviewed session
             |> Array.map ~f:Diff4_in_session.id
             |> Array.to_list
             |> List.map ~f:(fun id ->
               { Catch_up_session.Id_and_kind. id; kind }))
        | `Release ->
          let should_generate_catch_up =
            Staged.unstage
              (would_generate_catch_up_upon_release_if_not_reviewed t t.brain.brain)
          in
          Review_session.diff4s_in_session_not_implicitly_reviewed session
          |> Array.to_list
          |> List.filter_map ~f:(fun diff4_in_session ->
            Review_session.Diff4_in_session.diff4 diff4_in_session
            |> should_generate_catch_up
            |> Option.map ~f:(fun kind ->
              { Catch_up_session.Id_and_kind.
                id = Review_session.Diff4_in_session.id diff4_in_session
              ; kind
              }))
      in
      ok_exn (register_catch_up t query session ids);
      clear_current_session t;
;;

let mark_fully_reviewed t query reason =
  let review_goal = ok_known_exn t.review_goal in
  let indexed_diff4s = ok_known_exn t.indexed_diff4s in
  (match reason with
   | `Review_authorization review_authorization ->
     Review_authorization.requires_witness review_authorization
   | `Internal_mark__no_catch_up_allow_for_all -> ());
  commit_session_then_create_catch_up_to_goal_exn__allow_all t query ~reason;
  clear_current_session t;
  let diff4s =
    Brain_with_goal.diff4s_to_goal_internal t.brain review_goal
      ~indexed_diff4s ~reviewer:(reviewer t)
  in
  let mark_kind : Brain.Mark_kind.t =
    match reason with
    | `Internal_mark__no_catch_up_allow_for_all -> Internal__fully_reviewed
    | `Review_authorization _ -> User
  in
  set_brain t
    { Brain_with_goal.
      brain = Brain.extend t.brain.brain ~with_:diff4s ~reviewer:(reviewer t) ~mark_kind
    ; at_review_goal = Some review_goal
    }
;;

let release t query =
  commit_session_then_create_catch_up_to_goal_exn__allow_all t query ~reason:`Release;
  let pending = pending_now () in
  t.review_goal    <- pending;
  t.indexed_diff4s <- pending;
  forget_from_brain_internal_exn t ~what_to_forget:`All;
;;

let need_diff4s_starting_from t =
  let for_from_brain_to_goal = Brain.review_edges t.brain.brain in
  let for_from_session_end_to_goal =
    match t.current_session with
    | None -> Review_edge.Set.empty
    | Some session -> Brain.review_edges (brain_if_session_finished t session).brain
  in
  let for_from_goal_to_next_goal =
    (* While a bookmark update is pending, anyone can finish their review and start a new
       session that brings them to the current goal. Once they finish, it will be in their
       brain and if we didn't say we required diff4s starting from the goal we may miss
       diff4s.
       This is not subsumed by the similar computation that includes the review end of
       [Feature.review_goal feature], because just after a rebase,
       [Feature.review_goal feature] is an error whereas review managers still contain a
       valid review goal.
       Adding the review end of [Feature.review_goal feature] is not subsumed by this
       piece of code either, because there may be 0 review managers. *)
    match t.review_goal with
    | Pending_since _ | Known (Error _) -> Review_edge.Set.empty
    | Known (Ok goal) -> Review_edge.Set.singleton (Review_goal.review_edge goal)
  in
  Review_edge.Set.union_list
    [ for_from_brain_to_goal
    ; for_from_session_end_to_goal
    ; for_from_goal_to_next_goal
    ]
;;

let de_alias_brain t user_name_by_alias =
  let de_aliased_brain = Brain.de_alias t.brain.brain user_name_by_alias in
  if Brain.compare t.brain.brain de_aliased_brain = 0
  then `Nothing_to_do
  else
  if have_session_in_progress t
  then `Did_not_de_alias_due_to_review_session_in_progress
  else (
    clear_current_session t;
    set_brain t { brain = de_aliased_brain; at_review_goal = None };
    `De_aliased)
;;

let deserializer = Deserializer.with_serializer (fun serializer ->
  let open Deserializer.Let_syntax in
  let%map_open () = return ()
  and user_name = one (module Persist.User_name_file) ~in_file:user_name_file
  and crs = one (module Persist.Cr_comments) ~in_file:crs_file
  and current_session =
    one (module Persist.Current_session_id) ~in_file:current_session_id_file
    >>= function
    | None -> return None
    | Some review_session_id ->
      in_subdir (review_session_dir review_session_id) Review_session.deserializer
      >>| Option.return
  and brain = one (module Persist.Brain_file) ~in_file:brain_file
  in
  fun ~whole_feature_followers ~whole_feature_reviewers
    ~users_using_locked_sessions
    ~review_goal ~indexed_diff4s ~register_catch_up
    ~feature_cache_invalidator
    ~dynamic_upgrade_state ->
    Serializer.add_cache_invalidator serializer feature_cache_invalidator;
    let is_whole_feature_follower =
      Set.mem whole_feature_followers user_name in
    let is_whole_feature_reviewer =
      Set.mem whole_feature_reviewers user_name in
    let is_using_locked_sessions = Set.mem users_using_locked_sessions user_name in
    let review_goal =
      Or_error.map review_goal ~f:(fun review_goal ->
        Review_goal.restrict_to_may_review_or_follow review_goal
          (Reviewer.create user_name
             ~is_whole_feature_follower
             ~is_whole_feature_reviewer))
    in
    let current_session =
      Option.map current_session ~f:(fun f ->
        f ~user_name ~feature_cache_invalidator ~dynamic_upgrade_state)
    in
    let t =
      { user_name
      ; is_whole_feature_follower
      ; is_whole_feature_reviewer
      ; is_using_locked_sessions
      ; crs                                = undefined
      ; num_crs                            = undefined
      ; num_xcrs                           = undefined
      ; review_goal                        = Known review_goal
      ; indexed_diff4s
      ; brain                              = { brain; at_review_goal = None }
      ; current_session
      ; register_catch_up
      ; feature_cache_invalidator
      ; dynamic_upgrade_state
      ; serializer
      }
    in
    set_crs t crs ~persist:false;
    maybe_advance_brain t;
    t)
;;
