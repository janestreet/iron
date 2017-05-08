open! Core
open! Import

module Rpc = Async.Rpc

module Review_managers   = Hashtbl2_pair.Make (Feature_id)   (User_name)
module Catch_up_managers = Hashtbl2_pair.Make (Feature_path) (User_name)

module Fully_reviewed_edges =
  Hashtbl2_pair.Make (Rev.Compare_by_hash) (Rev.Compare_by_hash)
;;

module Fully_reviewed_revisions = Rev.Compare_by_hash.Hash_set

module Change_fully_reviewed_revisions_action =
  Iron_protocol.Change_fully_reviewed_revisions.Action

module Bookmark_without_feature = Iron_protocol.Bookmark_without_feature

module Bookmarks_without_feature = struct

  module Table = Hashtbl2_pair.Make (Remote_repo_path) (User_name)

  type t = Bookmark_without_feature.t list Table.t
  [@@deriving sexp_of]

  let create = Table.create
end

type t =
  { features                            : Feature.t Feature_forest.t
  ; features_by_id                      : Feature.t Feature_id.Table.t
  ; cr_soons                            : Cr_soons.t
  ; archived_features                   : Archived_features.t
  ; features_by_parties                 : Features_by_parties.t
  (* [review_managers] is doubly indexed.  The index by user is used to easily compute "fe
     todo" for a user.  The index by feature is used by Iron hydra when it notices a
     feature has updated and needs to update all the review managers. *)
  ; review_managers                     : Review_manager.t Review_managers.t
  ; catch_up_managers                   : Catch_up_manager.t Catch_up_managers.t
  (* [fully_reviewed_edges] is a directed graph where [(r1, r2)] means that all review
     as been done from [r1] to [r2]. *)
  ; fully_reviewed_edges                : Fully_reviewed_edge.t Fully_reviewed_edges.t
  ; fully_reviewed_revisions            : Fully_reviewed_revisions.t
  ; bookmarks_without_feature           : Bookmarks_without_feature.t
  ; user_info                           : User_info.t
  ; fact_db                             : Fact.Db.t
  ; server_config                       : Iron_config.t
  ; mutable serializer                  : Serializer.t option
  ; timed_event_table                   : Timed_event.Table.t
  ; cached_attributes_errors            : Error.t Queue.t
  ; rpc_stats                           : Rpc_stats.t
  ; metrics                             : Metrics.t
  ; dynamic_upgrade_state               : Dynamic_upgrade.State.t
  ; push_events                         : Push_events.t
  ; worker_cache                        : Worker_cache.t
  ; unclean_workspaces                  : Unclean_workspaces_manager.t
  ; event_subscriptions                 : Event_subscriptions.t
  ; feature_updates_manager             : Feature_updates_manager.t
  }
[@@deriving fields, sexp_of]

let find_feature t path = Feature_forest.find t.features path

let find_feature_exn t feature_path = ok_exn (find_feature t feature_path)

let find_feature_by_id t id = Hashtbl.find t.features_by_id id
let find_feature_by_id_exn t id = Hashtbl.find_exn t.features_by_id id

let find_root_by_remote_repo_path t remote_repo_path =
  with_return (fun r ->
    Feature_forest.iteri_roots t.features ~f:(fun _ feature ->
      match Feature.remote_repo_path feature with
      | None -> ()
      | Some remote_repo_path2 ->
        if Remote_repo_path.equal remote_repo_path remote_repo_path2
        then r.return (Ok feature));
    error "unknown remote repo path" remote_repo_path [%sexp_of: Remote_repo_path.t])
;;

let lines_required_to_separate_ddiff_hunks t feature_path ~fail_if_root_does_not_exist =
  let default = Constants.lines_required_to_separate_ddiff_hunks_default in
  let root_path = Feature_path.root_path feature_path in
  match find_feature t root_path with
  | Ok root -> Option.value (Feature.lines_required_to_separate_ddiff_hunks root) ~default
  | Error _ ->
    if fail_if_root_does_not_exist
    then raise_s [%sexp "No such root feature", (root_path : Feature_path.t)]
    else default
;;

let invariant t : unit =
  (* Because the sexp of the state can be so big, we don't do the usual [invariant] idiom:

     {[
       Invariant.invariant [%here] t <:sexp_of< t >> (fun () -> ...)
     ]}
  *)
  let check f = Invariant.check_field t f in
  Fields.iter
    ~features:(check (fun features ->
      Feature_forest.invariant Feature.invariant features;
      Feature_forest.iteri features ~f:(fun path feature ->
        assert (Hashtbl.mem t.features_by_id (Feature.feature_id feature));
        [%test_result: Feature_path.t] (Feature.feature_path feature) ~expect:path)))
    ~features_by_id:(check (fun features_by_id ->
      Hashtbl.iteri features_by_id ~f:(fun ~key ~data:feature ->
        assert (Feature_id.equal key (Feature.feature_id feature));
        let feature_from_forest =
          ok_exn (Feature_forest.find t.features (Feature.feature_path feature))
        in
        assert (phys_equal feature feature_from_forest))))
    ~archived_features:(check Archived_features.invariant)
    ~features_by_parties:(check Features_by_parties.invariant)
    ~cr_soons:(check Cr_soons.invariant)
    ~review_managers:(check (fun review_managers ->
      Hashtbl2_pair.invariant
        Feature_id.invariant User_name.invariant Review_manager.invariant
        review_managers;
      Hashtbl2_pair.iter review_managers
        ~f:(fun feature_id _ _ ->
          let feature = find_feature_by_id_exn t feature_id in
          assert (Feature_forest.mem t.features (Feature.feature_path feature)))))
    ~catch_up_managers:(check (fun catch_up_managers ->
      Hashtbl2_pair.invariant
        Feature_path.invariant User_name.invariant Catch_up_manager.invariant
        catch_up_managers))
    ~fully_reviewed_edges:
      (check
         (Hashtbl2_pair.invariant Rev.invariant Rev.invariant
            Fully_reviewed_edge.invariant))
    ~fully_reviewed_revisions:(check (Hash_set.iter ~f:Rev.invariant))
    ~fact_db:(check Fact.Db.invariant)
    ~serializer:(check (Option.iter ~f:Serializer.invariant))
    ~timed_event_table:(check Timed_event.Table.invariant)
    ~server_config:(check Iron_config.invariant)
    ~bookmarks_without_feature:(check (fun bookmarks_without_feature ->
      (* In theory, for:

         {[
           Hashtbl2_pair.find bookmarks_without_feature remote_repo_path user
           = Some [ ...; bookmark; ... ]
         ]}

         we should have:

         1.
         {[
           find_root_by_remote_repo_path t remote_repo_path = Ok root
         ]}

         and:

         2. [bookmark] should not name a feature under the [root].

         But in practice, (1) can be false when a root feature is archived, and (2) can be
         false transiently when a newly created feature is overwriting a bookmark but Iron
         hasn't received the information through synchronize-state yet. *)
      Hashtbl2_pair.invariant
        Remote_repo_path.invariant User_name.invariant ignore
        bookmarks_without_feature))
    ~user_info:(check User_info.invariant)
    ~cached_attributes_errors:ignore
    ~rpc_stats:(check Rpc_stats.invariant)
    ~metrics:(check Metrics.invariant)
    ~dynamic_upgrade_state:(check Dynamic_upgrade.State.invariant)
    ~push_events:(check Push_events.invariant)
    ~worker_cache:(check Worker_cache.invariant)
    ~unclean_workspaces:(check Unclean_workspaces_manager.invariant)
    ~event_subscriptions:(check Event_subscriptions.invariant)
    ~feature_updates_manager:(check Feature_updates_manager.invariant)
;;

let iter_review_managers_of_user t user ~f =
  Hashtbl2_pair.find2_iter1 t.review_managers user ~f
;;

let iter_review_managers_of_feature t feature ~f =
  let feature_id = Feature.feature_id feature in
  Hashtbl2_pair.find1_iter2 t.review_managers feature_id ~f
;;

let exists_review_manager t feature ~f =
  with_return (fun r ->
    iter_review_managers_of_feature t feature  ~f:(fun user review_manager ->
      if f user review_manager then r.return true);
    false)
;;

let iter_catch_up_managers_of_user t user ~f =
  Hashtbl2_pair.find2_iter1 t.catch_up_managers user ~f
;;

let iter_catch_up_managers_of_feature_path t feature_path ~f =
  Hashtbl2_pair.find1_iter2 t.catch_up_managers feature_path ~f
;;

(* Here is the file-system layout of a persistent [t].

   {v
     fact-actions
     features/
       $FEATURE_ID/
         creation
         queries
         review-managers/
           $USER/
             creation
             crs
             queries
     archived-features/
       ... <serialized Archived_features.t> ...
     catch-up/
       $USER/
         $SESSION_ID/
           creation
           queries
     event-subscriptions/
     dynamic-upgrade/
      state
     push-events/
      properties
     user-info/
      aliases
      existing-users
      typos
      valid-users
     worker-cache/
      properties
     unclean-workspaces/
      user1
      user2
   v}
*)

let archived_features_dir              = Relpath.of_string "archived-features"
let features_dir                       = Relpath.of_string "features"
let user_info_dir                      = Relpath.of_string "user-info"
let fully_reviewed_edges_file          = Relpath.of_string "fully-reviewed-edges"
let fully_reviewed_revisions_queries_file =
  Relpath.of_string "fully-reviewed-revisions-queries"
let review_managers_dir_in_feature_dir = Relpath.of_string "review-managers"
let fact_actions_file                  = Relpath.of_string "fact-actions"
let catch_up_dir                       = Relpath.of_string "catch-up"
let worker_cache_dir                   = Relpath.of_string "worker-cache"
let push_events_dir                    = Relpath.of_string "push-events"
let unclean_workspaces_dir             = Relpath.of_string "unclean-workspaces"
let event_subscriptions_dir            = Relpath.of_string "event-subscriptions"
let dynamic_upgrade_dir                = Relpath.of_string "dynamic-upgrade"


module Persist = struct
  module Fully_reviewed_edges = struct
    include Persistent.Make
        (struct let version = 1 end)
        (Fully_reviewed_edge.Stable.V1)
  end
  module Change_fully_reviewed_revisions_query = struct
    include Persistent.Make
        (struct let version = 1 end)
        (struct
          type t = Iron_protocol.Change_fully_reviewed_revisions.Stable.What_to_do.V1.t
                     Query.Stable.V1.t
          [@@deriving sexp]
        end)
  end
end

let feature_dir feature_id =
  Relpath.extend features_dir (Feature_id.to_file_name feature_id)
;;

let serializer_exn t =
  match t.serializer with
  | Some s -> s
  | None -> failwith "serializer isn't defined"
;;

let create_feature_serializer t uuid =
  Serializer.relativize (serializer_exn t) ~dir:(feature_dir uuid)
;;

let archived_feature_dir archived_feature =
  Relpath.append archived_features_dir
    (Archived_features.feature_dir archived_feature)
;;

let review_managers_dir feature_id =
  Relpath.append (feature_dir feature_id) review_managers_dir_in_feature_dir
;;

let review_manager_dir feature_id reviewer =
  Relpath.extend (review_managers_dir feature_id) (User_name.to_file_name reviewer)
;;

let catch_up_user_dir user_name =
  Relpath.extend catch_up_dir (User_name.to_file_name user_name)
;;

let catch_up_session_dir user_name id =
  Relpath.extend (catch_up_user_dir user_name) (Session_id.to_file_name id)
;;

let add_fully_reviewed_edge t fully_reviewed_edge =
  let key1 = Fully_reviewed_edge.from_ fully_reviewed_edge in
  let key2 = Fully_reviewed_edge.to_   fully_reviewed_edge in
  if not (Hashtbl2_pair.mem t.fully_reviewed_edges key1 key2)
  then (
    Hashtbl2_pair.add_exn t.fully_reviewed_edges key1 key2 fully_reviewed_edge;
    match t.serializer with
    | None -> ()
    | Some serializer ->
      Serializer.append_to serializer fully_reviewed_edge ~file:fully_reviewed_edges_file
        (module Persist.Fully_reviewed_edges))
;;

let is_fully_reviewed_edge t ~base ~tip =
  Hashtbl2_pair.mem t.fully_reviewed_edges base tip
;;

let set_feature_parties t feature =
  List.iter Features_by_parties.Parties.all ~f:(fun parties ->
    Features_by_parties.set_users t.features_by_parties
      (Feature.feature_id feature) parties
      (match parties with
       | Owners                  -> Feature.owners feature |> User_name.Set.of_list
       | Whole_feature_followers -> Feature.whole_feature_followers feature))
;;

let find_review_manager t feature user =
  let feature_id = Feature.feature_id feature in
  let feature_path = Feature.feature_path feature in
  match Hashtbl2_pair.find t.review_managers feature_id user with
  | Some x -> Ok x
  | None ->
    error "no CRs or review work" (`for_ user, `in_feature feature_path)
      [%sexp_of: [ `for_ of User_name.t ] * [ `in_feature of Feature_path.t ]]
;;

let add_review_manager t feature review_manager =
  Hashtbl2_pair.add_exn t.review_managers
    (Feature.feature_id feature)
    (Review_manager.user_name review_manager)
    review_manager
;;

let iter_review_managers_of_feature t ?of_:(whose_review_managers = `All_users)
      feature ~f =
  match whose_review_managers with
  | `User user ->
    f user (ok_exn (find_review_manager t feature user))
  | `All_users ->
    iter_review_managers_of_feature t feature ~f
  | `All_users_but users_to_skip ->
    iter_review_managers_of_feature t feature ~f:(fun user_name review_manager ->
      if not (Set.mem users_to_skip user_name)
      then f user_name review_manager)
;;

let remote_repo_path ?rev_zero t feature =
  let feature_path = Feature.feature_path feature in
  let root_feature = ok_exn (Feature_forest.root_of t.features feature_path) in
  let remote_repo_path =
    match Feature.remote_repo_path root_feature with
    | Some x -> x
    | None ->
      raise_s [%sexp "Iron bug: root feature missing remote_repo_path"
                   , (Feature.feature_path root_feature : Feature_path.t)]
  in
  let feature_rev_zero = Feature.rev_zero feature in
  (match rev_zero with
   | None -> ()
   | Some rev_zero ->
     if not (Rev.equal_node_hash rev_zero feature_rev_zero)
     then repo_mismatch remote_repo_path);
  remote_repo_path
;;

(* [find_parent] does not distinguish between these cases:

   1. [feature_path] is a root and should is expected to have no parent

   2. [feature_path] has a parent but we can't find it

   In new code we should consider whether (2) is a bug, and if so use [find_parent_exn].
*)
let find_parent t feature_path =
  match Feature_path.parent feature_path with
  | Ok parent_path -> find_feature t parent_path
  | Error _ as e -> e
;;

let find_parent_exn t feature_path =
  match Feature_path.parent feature_path with
  | Ok parent_path -> Some (ok_exn (find_feature t parent_path))
  | Error _ -> None
;;

let feature_crs t feature =
  let feature_id = Feature.feature_id feature in
  match Hashtbl2_pair.mem1 t.review_managers feature_id with
  | false -> Ok []
  | true ->
    with_return (fun r ->
      let crs = ref [] in
      Hashtbl2_pair.find1_iter2 t.review_managers feature_id
        ~f:(fun _ review_manager ->
          match Review_manager.crs review_manager with
          | Error _ as e -> r.return e
          | Ok l -> crs := l @ !crs);
      Ok !crs)
;;

let feature_cr_summary t feature =
  let alternate_names = User_info.alternate_names t.user_info ~which:`All in
  Or_error.map (feature_crs t feature) ~f:(fun crs ->
    Cr_comment.Summary.create crs
      ~feature_owner:(Feature.first_owner feature) ~alternate_names)
;;

let users_with_existing_review_manager t feature =
  match Hashtbl2_pair.find1 t.review_managers (Feature.feature_id feature) with
  | None -> User_name.Set.empty
  | Some hashtbl -> User_name.Set.of_list (Hashtbl.keys hashtbl)
;;

let find_catch_up_manager t feature_path user_name =
  match Hashtbl2_pair.find t.catch_up_managers feature_path user_name with
  | Some x -> x
  | None ->
    let catch_up_manager = Catch_up_manager.create () in
    Hashtbl2_pair.add_exn t.catch_up_managers
      feature_path
      user_name
      catch_up_manager;
    catch_up_manager
;;

let rename_catch_up_sessions t query ~from_feature ~to_ =
  let from = Feature.feature_path from_feature in
  let from_id = Feature.feature_id from_feature in
  let to_rename = ref [] in
  iter_catch_up_managers_of_feature_path t from ~f:(fun user catch_up_manager ->
    match Catch_up_manager.remove_all_links_for_feature_id catch_up_manager from_id with
    | [] -> ()
    | catch_up_sessions -> to_rename := (user, catch_up_sessions) :: !to_rename);
  List.iter !to_rename ~f:(fun (user, catch_up_sessions) ->
    let catch_up_manager = find_catch_up_manager t to_ user in
    List.iter catch_up_sessions ~f:(fun catch_up_session ->
      Catch_up_session.set_feature_path catch_up_session query to_;
      Catch_up_manager.add catch_up_manager catch_up_session));
;;

let catch_up_in_session t query feature_path catch_up_manager catch_up_session
      diff4_in_session_ids for_ =
  let result =
    Catch_up_session.catch_up catch_up_session query diff4_in_session_ids
  in
  if Catch_up_session.all_diff4s_are_caught_up catch_up_session
  then (
    let id = Catch_up_session.id catch_up_session in
    Catch_up_manager.remove_link catch_up_manager id;
    Serializer.remove_subtree (serializer_exn t) ~dir:(catch_up_session_dir for_ id);
    if Catch_up_manager.is_empty catch_up_manager
    then (
      Hashtbl2_pair.remove_exn t.catch_up_managers feature_path for_;
      if not (Hashtbl2_pair.mem2 t.catch_up_managers for_) then
        Serializer.remove_subtree (serializer_exn t) ~dir:(catch_up_user_dir for_)));
  ok_exn result
;;

let make_register_catch_up t feature for_ =
  { Review_manager.Register_catch_up. f = fun query review_session ids_with_reason ->
      let feature_path = Feature.feature_path feature in
      let catch_up_manager = find_catch_up_manager t feature_path for_ in
      let session_id = Review_session.id review_session in
      let catch_up_session =
        match Catch_up_manager.find catch_up_manager session_id with
        | Some catch_up_session -> catch_up_session
        | None ->
          let remote_repo_path = remote_repo_path t feature in
          let catch_up_session =
            Catch_up_session.create
              ~review_session
              ~remote_rev_zero:(Feature.rev_zero feature)
              ~remote_repo_path
              ~feature_path
              ~feature_id:(Feature.feature_id feature)
              ~whole_feature_reviewers:
                (Feature.whole_feature_reviewers feature)
              ~owners:(Feature.owners feature)
              ~base:(Feature.base feature)
              ~tip:(Feature.tip feature)
              ~description:(Feature.description feature)
              ~is_permanent:(Feature.is_permanent feature)
              ~seconder:(Feature.seconder feature)
              (Serializer.relativize (serializer_exn t)
                 ~dir:(catch_up_session_dir for_ session_id))
          in
          Catch_up_manager.add catch_up_manager catch_up_session;
          catch_up_session
      in
      Catch_up_session.reviewed catch_up_session query ids_with_reason
  }
;;

let create_review_manager t feature user ~cr_comments =
  let review_manager : Review_manager.t =
    Review_manager.create user
      ~is_whole_feature_follower:(Set.mem (Feature.whole_feature_followers feature) user)
      ~is_whole_feature_reviewer:(Set.mem (Feature.whole_feature_reviewers feature) user)
      ~is_using_locked_sessions:(User_info.Using_locked_sessions.mem t.user_info user)
      ~cr_comments
      ~base_facts:(Feature.base_facts feature)
      ~register_catch_up:(make_register_catch_up t feature user)
      ~feature_cache_invalidator:(Feature.cache_invalidator feature)
      ~dynamic_upgrade_state:t.dynamic_upgrade_state
      (Serializer.relativize (serializer_exn t)
         ~dir:(review_manager_dir (Feature.feature_id feature) user))
  in
  add_review_manager t feature review_manager;
  review_manager
;;

module Feature_email_sent_upon = struct
  type t = Iron_protocol.Get_feature_email_recipients.Sent_upon.t =
    | Archive
    | Release
    | Release_into
  [@@deriving enumerate, sexp_of]

  let of_send_email_upon : Send_email_upon.t -> t = function
    | Archive -> Archive
    | Release -> Release
  ;;

  let to_send_email_upon : t -> Send_email_upon.t option = function
    | Archive      -> Some Archive
    | Release      -> Some Release
    | Release_into -> None
  ;;


  let%test "Send_email_upon.t round trip via t" =
    List.for_all Send_email_upon.all ~f:(fun t ->
      [%compare.equal: Send_email_upon.t] t
        (t |> of_send_email_upon |> to_send_email_upon |> Option.value_exn))
  ;;

  let%test "we need two types" =
    let ts = List.map all ~f:to_send_email_upon in
    List.exists ts ~f:Option.is_some
    && List.exists ts ~f:Option.is_none
  ;;
end

let feature_email feature ~(sent_upon : Feature_email_sent_upon.t) =
  let users us =
    us
    |> List.map ~f:Email_address.of_user_name
    |> Email_address.Set.of_list
  in
  let feature_send_email_to =
    let should_add_send_email_to =
      match Feature_email_sent_upon.to_send_email_upon sent_upon with
      | None      -> false
      | Some upon -> Set.mem (Feature.send_email_upon feature) upon
    in
    if should_add_send_email_to
    then Feature.send_email_to feature
    else Email_address.Set.empty
  in
  Email_address.Set.union_list
    [ feature_send_email_to
    ; users (Feature.seconder feature |> Option.to_list)
    ; users (Feature.owners feature)
    ; users (Set.to_list (Feature.whole_feature_followers feature))
    ; users (Set.to_list (Feature.whole_feature_reviewers feature))
    ]
;;

let seconding_allowed feature ~by ~even_though_empty ~even_though_owner
      ~even_if_locked =
  try (
    Option.iter (Feature.seconder feature) ~f:(fun seconder ->
      raise_s [%sexp "feature is already seconded by", (seconder : User_name.t)]);
    (match Feature.is_empty feature, even_though_empty with
     | false, false | true, true -> ()
     | false, true  ->
       failwithf "cannot use %s with nonempty feature" Switch.even_though_empty ()
     | true , false -> failwith "feature is empty");
    (match Feature.is_owner feature by, even_though_owner with
     | false, false | true, true -> ()
     | false, true ->
       raise_s
         [%sexp
           (sprintf "cannot use %s with a non-owner" Switch.even_though_owner : string)
         , (by : User_name.t)
         ]
     | true , false ->
       if not (Feature.all_whole_feature_reviewers_are_owners feature)
       then failwith "please have a non-owner second"
       else if Set.length (Feature.whole_feature_reviewers feature) <= 1
       then failwith "please use [fe change] to add a whole-feature reviewer");
    (if not (Set.mem (Feature.whole_feature_reviewers feature) by)
     then failwith "not a whole-feature reviewer");
    (if not (Feature.review_is_enabled feature) then failwith "review is not enabled");
    (match Feature.review_goal feature with
     | Ok _ -> ()
     | Error e -> raise_s [%sexp "feature has problems", (e : Error.t)]);
    (if not even_if_locked
     then ok_exn (Feature_locks.check_unlocked (Feature.locks feature) Second));
    Ok ())
  with exn ->
    error "cannot second" exn [%sexp_of: exn]
;;

let some_whole_feature_reviewer_can_make_progress t feature =
  Set.exists (Feature.whole_feature_reviewers feature)
    ~f:(fun user ->
      match find_review_manager t feature user with
      | Error _ -> false
      | Ok review_manager ->
        Review_manager.can_make_progress review_manager Entire_goal)
;;

let seconding_is_recommended t ~for_ feature ~even_if_locked =
  match
    seconding_allowed feature ~by:for_ ~even_though_empty:false ~even_though_owner:false
      ~even_if_locked
  with
  | Error _ -> false
  | Ok () ->
    Feature.tip_is_cr_clean feature
    && (match Feature.next_bookmark_update feature with
      | No_update_expected -> true
      | Update_expected_since _ | No_update_expected_due_to_iron_bug _ -> false)
    && not (some_whole_feature_reviewer_can_make_progress t feature)
;;

let incr_catch_up_count table key catch_up_manager =
  let num_lines_to_catch_up =
    Catch_up_manager.line_count_remaining_to_catch_up catch_up_manager
  in
  if Line_count.Catch_up.total num_lines_to_catch_up > 0
  then
    Hashtbl.update table key ~f:(function
      | None -> num_lines_to_catch_up
      | Some previous_value ->
        Line_count.Catch_up.(previous_value + num_lines_to_catch_up)
    )
;;

let build_catch_up_lines_by_feature t for_ =
  let catch_up_table = Feature_path.Table.create () in
  iter_catch_up_managers_of_user t for_ ~f:(incr_catch_up_count catch_up_table);
  catch_up_table
;;

let build_catch_up_lines_by_user t feature =
  let catch_up_table = User_name.Table.create () in
  let feature_path = Feature.feature_path feature in
  iter_catch_up_managers_of_feature_path t feature_path
    ~f:(incr_catch_up_count catch_up_table);
  catch_up_table
;;

module Completed_review = struct

  type data = { have_review_session_in_progress : bool }
  and t = data User_name.Table.t
  [@@deriving sexp_of]

  let create () = User_name.Table.create ()

  let filter t ~f =
    Hashtbl.to_alist t
    |> List.filter_map ~f:(fun (user, { have_review_session_in_progress }) ->
      Option.some_if (f have_review_session_in_progress) user)
    |> User_name.Set.of_list
  ;;

  let users_with_review_session_in_progress t = filter t ~f:Fn.id
  let users_without_review_session_in_progress t = filter t ~f:(not)

  let add t user ~have_review_session_in_progress =
    Hashtbl.set t ~key:user ~data:{ have_review_session_in_progress }
  ;;
end

let compute_review_analysis state feature : Review_analysis.t option =
  match Feature.diff_from_base_to_tip feature with
  | Pending_since _ | Known (Error _) -> None
  | Known (Ok diff2s) ->
    let completed_review_by_diff2 =
      Diff2.Ignoring_rev.Table.of_alist_exn
        (List.map diff2s ~f:(fun diff2 -> (diff2, Completed_review.create ())))
    in
    let mark_as_completed_review user diff2 ~have_review_session_in_progress =
      match Hashtbl.find completed_review_by_diff2 diff2 with
      | None -> ()
      | Some completed_review ->
        Completed_review.add completed_review user ~have_review_session_in_progress
    in
    let users_with_review_session_in_progress = User_name.Hash_set.create () in
    iter_review_managers_of_feature state feature ~f:(fun user review_manager ->
      let have_review_session_in_progress =
        Review_manager.have_session_in_progress review_manager
      in
      (if have_review_session_in_progress
       then (
         Hash_set.add users_with_review_session_in_progress user;
         (* When the review obligations are satisfied by k-of-n we stop asking the
            remaining (n-k) users to review that diff.  We do not want to this to flicker
            each time one of the [k] starts a new session.  This is not a statement of
            what they have in their brain, because of the distinction ensured by the flag
            [has_uncommitted_session:true] *)
         List.iter
           (Review_manager.reviewed_diff4s_output_in_current_session review_manager)
           ~f:(mark_as_completed_review user ~have_review_session_in_progress:true)));
      List.iter (Review_manager.brain review_manager) ~f:(fun { diff2; _ } ->
        mark_as_completed_review user diff2 ~have_review_session_in_progress
      );
    );
    let users_with_review_session_in_progress =
      User_name.Set.of_hash_set users_with_review_session_in_progress
    in
    let diff2s =
      Hashtbl.mapi completed_review_by_diff2 ~f:(fun ~key:diff2 ~data:completed_review ->
        let files_obligations_analysis completed_review =
          let completed_review_satisfies_files_obligations_in f =
            match (f diff2).Attributed_file.attributes with
            | `Absent -> true
            | `Present attributes ->
              Review_obligation.is_satisfied attributes.review_obligation
                ~by:completed_review
          in
          { Review_analysis.Attributed_files_analysis.
            completed_review
          ; completed_review_satisfies_non_wfr_obligations_in_base =
              completed_review_satisfies_files_obligations_in Diff2.base
          ; completed_review_satisfies_non_wfr_obligations_in_tip =
              completed_review_satisfies_files_obligations_in Diff2.tip
          }
        in
        let actually_completed_review =
          Completed_review.users_without_review_session_in_progress completed_review
        in
        let assuming_expected_users_are_finished =
          User_name.Set.union_list
            [ actually_completed_review
            ; Completed_review.users_with_review_session_in_progress completed_review
            ; Feature.whole_feature_reviewers feature
            ]
        in
        { Review_analysis.Diff2_analysis.
          actually_reviewed =
            files_obligations_analysis actually_completed_review
        ; assuming_expected_users_are_finished =
            files_obligations_analysis assuming_expected_users_are_finished
        })
      |> Hashtbl.to_alist
      |> Diff2.Ignoring_rev.Map.of_alist_exn
    in
    let whole_feature_reviewers =
      Feature.whole_feature_reviewers feature
      |> Set.to_map ~f:(fun reviewer ->
        { Review_analysis.Whole_feature_reviewer_analysis.
          obligations_are_satisfied =
            match find_review_manager state feature reviewer with
            | Error _ -> false
            | Ok review_manager ->
              not (Review_manager.have_session_in_progress review_manager)
              && (match
                    Review_manager.line_count_remaining_to_review
                      review_manager Entire_goal
                  with
                  | Partially_known _ -> false
                  | Fully_known { must_review; _ } -> must_review = 0)
        })
    in
    Some { diff2s
         ; users_with_review_session_in_progress
         ; whole_feature_reviewers
         }
;;

let what_goal_subset_needs_to_be_reviewed feature : Review_manager.Goal_subset.t =
  match Feature.review_analysis feature with
  | None                 -> Entire_goal
  | Some review_analysis -> Entire_goal_but_empty_if_satisfied_by review_analysis
;;

let compute_line_count_by_user_exn t feature =
  let catch_up_lines_by_user = build_catch_up_lines_by_user t feature in
  let line_count_by_user =
    feature
    |> Feature.line_count_by_user
    |> ok_exn
    |> Map.to_alist
    |> List.map ~f:(fun (user, { review
                               ; completed
                               ; have_potentially_blocking_review_session_in_progress
                               } ) ->
                     let catch_up =
                       Hashtbl.find_and_remove catch_up_lines_by_user user
                       |> Option.value ~default:Line_count.Catch_up.zero
                     in
                     user,
                     { Line_count.
                       review = To_goal_via_session.fully_known_exn review
                     ; catch_up
                     ; completed
                     ; have_potentially_blocking_review_session_in_progress
                     })
  in
  (* At this point, we found and removed all entries from [catch_up_lines_by_user] that
     had a review_manager.  However, there might be more remaining. *)
  let catch_up_for_features_with_no_review_remaining =
    List.map (Hashtbl.to_alist catch_up_lines_by_user) ~f:(fun (user, catch_up) ->
      user, Line_count.catch_up_only catch_up)
  in
  List.concat
    [ line_count_by_user
    ; catch_up_for_features_with_no_review_remaining
    ]
  |> List.filter ~f:(fun (_, line_count) -> not (Line_count.is_zero line_count))
  |> User_name.Map.of_alist_exn
  |> Map.to_alist
;;

module Trying_to = struct
  type t =
    | Release
    | Release_into
  [@@deriving sexp_of]
end

let rec next_steps_gen t feature ~(trying_to : Trying_to.t) : Next_step.t list =
  match Feature.has_bookmark feature with
  | false -> [ Restore_bookmark ]
  | true ->
    match Feature.next_bookmark_update feature with
    | No_update_expected_due_to_iron_bug _ -> [ Report_iron_bug ]
    | No_update_expected | Update_expected_since _ as next_bookmark_update ->
      let am_expecting_bookmark_update =
        Next_bookmark_update.am_expecting_bookmark_update next_bookmark_update
      in
      let am_expecting_base_update =
        match Feature.next_base_update feature with
        | Update_expected _  -> true
        | No_update_expected -> false
      in
      let am_continuously_releasing_into =
        match trying_to with
        | Release -> false
        | Release_into ->
          match Feature.release_process feature with
          | Continuous -> true
          | Direct -> false
      in
      let could_wait_for_hydra =
        am_expecting_bookmark_update
        && not am_continuously_releasing_into
      in
      let fix_problems () : Next_step.t list =
        if am_expecting_bookmark_update
        then [ Wait_for_hydra ]
        else [ Fix_problems ]
      in
      match Feature.review_goal feature with
      | Error _ -> fix_problems ()
      | Ok _review_goal ->
        match feature_crs t feature with
        | Error _ -> fix_problems ()
        | Ok crs ->
          let maybe_prepend_rebase next_steps : Next_step.t list =
            match trying_to with
            | Release_into -> next_steps
            | Release ->
              let is_rebased =
                match find_parent t (Feature.feature_path feature) with
                | Error _   -> true
                | Ok parent ->
                  Rev.equal_node_hash (Feature.tip parent) (Feature.base feature)
              in
              if is_rebased
              then next_steps
              else if is_error (Feature_locks.check_unlocked
                                  (Feature.locks feature) Rebase)
              then Unlock Rebase :: next_steps
              else Rebase        :: next_steps
          in
          let what_goal_subset_needs_to_be_reviewed =
            what_goal_subset_needs_to_be_reviewed feature
          in
          let someone_reviewing_can_make_progress =
            exists_review_manager t feature ~f:(fun user review_manager ->
              Feature.user_is_currently_reviewing feature user
              && Review_manager.can_make_progress
                   review_manager what_goal_subset_needs_to_be_reviewed)
          in
          if not (List.is_empty crs)
          then
            if someone_reviewing_can_make_progress
            then [ CRs; Review ]
            else [ CRs ]
          else (
            let is_fully_reviewed =
              match Feature.review_analysis feature with
              | None -> false
              | Some review_analysis ->
                Review_analysis.obligations_are_satisfied review_analysis
            in
            (* [someone_reviewing_can_make_progress => not is_fully_reviewed]
               actually doesn't hold because people reviewing ownership change only don't
               block release *)
            if not is_fully_reviewed && someone_reviewing_can_make_progress
            then [ Review ]
            (* We report [Wait_for_hydra] only if no [CRs] or [Review], to avoid
               flickering due to bookmark updates. *)
            else if could_wait_for_hydra
            then [ Wait_for_hydra ]
            (* We report [Add_code] after [Wait_for_hydra] so that as soon as the first
               commit is pushed, we report [Wait_for_hydra] rather than [Add_code]. *)
            else if Feature.is_empty feature
                 && (match trying_to with Release -> true | Release_into -> false)
            then maybe_prepend_rebase [ Add_code ]
            (* We report [Enable_review] after [Add_code] so that we don't encourage
               enabling review on empty features. *)
            else if not (Feature.review_is_enabled feature)
            then maybe_prepend_rebase [ Enable_review ]
            else if not (Feature.is_seconded feature)
            then (
              let ask_seconder_or_unlock_seconding feature : Next_step.t list =
                match Feature_locks.check_unlocked (Feature.locks feature) Second with
                | Ok   () -> [ Ask_seconder ]
                | Error _ -> [ Unlock Second ]
              in
              if am_expecting_bookmark_update
              then [ Wait_for_hydra ]
              else if Set.length (Feature.whole_feature_reviewers feature) <=1
              then maybe_prepend_rebase [ Add_whole_feature_reviewer ]
              else if Set.exists (Feature.whole_feature_reviewers feature) ~f:(fun for_ ->
                seconding_is_recommended t ~for_ feature ~even_if_locked:true)
              then maybe_prepend_rebase (ask_seconder_or_unlock_seconding feature)
              else if Feature.is_empty feature
                   && (match trying_to with
                     | Release -> false
                     | Release_into -> true)
              then ask_seconder_or_unlock_seconding feature
              else maybe_prepend_rebase [ Widen_reviewing ])
            else if not is_fully_reviewed
            then (
              if exists_review_manager t feature
                   ~f:(fun user review_manager ->
                     not (Feature.user_is_currently_reviewing feature user)
                     && Review_manager.can_make_progress review_manager
                          what_goal_subset_needs_to_be_reviewed)
              then maybe_prepend_rebase [ Widen_reviewing ]
              else if am_expecting_bookmark_update
              then [ Wait_for_hydra ]
              else if am_expecting_base_update
              then [ Restore_base ]
              else
                (* We can reach here if there is an unsatisfiable review obligation.
                   [Reviewed_by.eval] is supposed to prevent such things. *)
                [ Report_iron_bug ])
            (* If we reach here, the feature is seconded, fully reviewed, and CR clean. *)
            else if not (Feature.is_seconded feature)
                 || not is_fully_reviewed
                 || not (List.is_empty crs)
            then (
              if am_expecting_base_update
              then [ Restore_base ]
              else [ Report_iron_bug ])
            else (
              let lock_name : Next_step.Lock_name.t =
                match trying_to with
                | Release      -> Release
                | Release_into -> Release_into
              in
              if is_error (Feature_locks.check_unlocked
                             (Feature.locks feature)
                             (Next_step.Lock_name.to_lock_name lock_name))
              then [ Unlock lock_name ]
              else (
                match trying_to with
                | Release_into -> []
                | Release ->
                  match Feature_path.parent (Feature.feature_path feature) with
                  | Error _ -> [ Release ]
                  | Ok parent_path ->
                    let parent = find_feature_exn t parent_path in
                    match next_steps_gen t parent ~trying_to:Release_into with
                    | [] -> maybe_prepend_rebase [ Release ]
                    | parent_next_step :: _ ->
                      let next_steps = [ Next_step.In_parent parent_next_step ] in
                      match parent_next_step with
                      | Wait_for_hydra ->
                        (* We don't want to encourage rebasing if the parent tip is about
                           to change. *)
                        next_steps
                      | _ -> maybe_prepend_rebase next_steps)))
;;

module Next_steps = struct
  type t = Next_step.t list [@@deriving sexp_of]

  let equal t1 t2 = List.equal t1 t2 ~equal:Next_step.equal
end

module Review_analysis_opt = struct
  type t = Review_analysis.t option [@@deriving sexp_of]

  let equal t1 t2 = Option.equal Review_analysis.equal t1 t2
end

let initialize_cached_feature_attributes t feature =
  let line_count_by_user =
    Cached.create
      (module Line_count.Cached_in_feature.By_user)
      ~compute_depends_on:(fun () -> [ Feature.cache_invalidator feature ])
      ~compute_result:(fun () ->
        let goal_subset = what_goal_subset_needs_to_be_reviewed feature in
        alist_of_iter (iter_review_managers_of_feature t feature)
        |> List.map ~f:(fun (user, review_manager) ->
          user, Review_manager.line_count_cached_in_feature review_manager goal_subset)
        |> User_name.Map.of_alist_exn)
  in
  let next_steps =
    let compute_depends_on () =
      (match find_parent t (Feature.feature_path feature) with
       | Error _   -> [ feature ]
       | Ok parent -> [ feature; parent ])
      |> List.map ~f:Feature.cache_invalidator
    in
    Cached.create
      (module Next_steps)
      ~compute_depends_on
      ~compute_result:(fun () -> next_steps_gen t feature ~trying_to:Release)
  in
  let review_analysis =
    Cached.create
      (module Review_analysis_opt)
      ~compute_depends_on:(fun () -> [ Feature.cache_invalidator feature ])
      ~compute_result:(fun () -> compute_review_analysis t feature)
  in
  Feature.set_cached_attributes feature
    ~line_count_by_user
    ~next_steps
    ~review_analysis
;;

let parent_release_process t feature : Release_process.t =
  match find_parent t (Feature.feature_path feature) with
  | Error _   -> Direct
  | Ok parent -> Feature.release_process parent
;;

let check_releasable t feature ~how_to_release ~trust_cached_attributes =
  (* For robustness, we clear all cached attributes in the feature to force every value
     that might be used by [check_releasable] to be recomputed. *)
  if not trust_cached_attributes then Feature.clear_cached_attributes feature;
  let next_steps = Feature.next_steps feature in
  let not_releasable () =
    let feature_path = Feature.feature_path feature in
    let next_steps =
      List.filter next_steps ~f:(function
        | Release
        | Wait_for_continuous_release -> false
        | _ -> true)
    in
    let or_lock_error =
      Or_error.combine_errors_unit
        (List.map next_steps ~f:(function
           | Unlock lock_name ->
             Feature_locks.check_unlocked (Feature.locks feature)
               (Next_step.Lock_name.to_lock_name lock_name)
           | In_parent (Unlock lock_name) ->
             let parent = ok_exn (find_parent t feature_path) in
             Feature_locks.check_unlocked (Feature.locks parent)
               (Next_step.Lock_name.to_lock_name lock_name)
           | _ -> Ok ()))
    in
    (* users have been asking for a better error message here, trying to limit noise with
       fields shown as "Ok ()" *)
    let error_opt =
      match or_lock_error with
      | Ok () -> None
      | Error e -> Some e
    in
    error_s
      [%sexp
        "feature is not releasable",
        { feature    = (feature_path : Feature_path.t)
        ; next_steps = (next_steps   : Next_steps.t)
        ; locked     = (error_opt    : Error.t sexp_option)
        }
      ]
  in
  match next_steps with
  | [ Release ] -> Ok ()
  | [ Wait_for_hydra ]
  | [ Rebase; Release ]
  | [ Wait_for_continuous_release ] ->
    (match how_to_release with
     | `Push_to_hydra -> Ok ()
     | `Directly -> not_releasable ())
  | _ -> not_releasable ()
;;

let users_with_unclean_workspaces t feature_path =
  Unclean_workspaces_manager.find_feature t.unclean_workspaces feature_path
;;

let feature_to_protocol ?rev_zero t feature : Iron_protocol.Feature.t =
  let feature_path = Feature.feature_path feature in
  let line_count_by_user =
    Or_error.try_with (fun () ->
      compute_line_count_by_user_exn t feature)
  in
  let is_rebased =
    match find_parent t feature_path with
    | Error _   -> true
    | Ok parent -> Rev.equal_node_hash (Feature.tip parent) (Feature.base feature)
  in
  Feature.to_protocol feature
    ~remote_repo_path:(remote_repo_path t feature ?rev_zero)
    ~has_children:(Feature_forest.has_children_exn t.features feature_path)
    ~cr_summary:(feature_cr_summary t feature)
    ~line_count_by_user
    ~is_archived:No
    ~is_rebased
    ~users_with_unclean_workspaces:(users_with_unclean_workspaces t feature_path)
;;

module Post_check_in_features : sig
  type state
  type 'a t
  type 'a f = state -> 'a -> [ `run_after_reaction of (unit -> Which_features.t) ]

  val create : 'a f -> 'a t
  val which_features : 'a t -> 'a f

  val all  : _ t
  val none : _ t

  val these_features     : ('a -> Which_features.t) -> 'a t

  val one_if_present     : ('a -> Feature_path.t) -> 'a t
  val one                : ('a -> Feature_path.t) -> 'a t

  val rename             : to_:('a -> Feature_path.t) -> 'a t
  val compress           : ('a -> Feature_path.t) -> 'a t

  val all_review_managers_of_users : ('a -> User_name.Set.t) -> 'a t
end with type state := t = struct
  type state = t
  type 'a t = state -> 'a -> [ `run_after_reaction of (unit -> Which_features.t) ]
  type 'a f = 'a t

  let create f = f
  let which_features t state a = t state a

  let all _ _ = `run_after_reaction (const Which_features.All_features)

  let none _ _ = `run_after_reaction (const (Which_features.Features [ ]))

  let these_features which_features _ action =
    `run_after_reaction (fun () -> which_features action)
  ;;

  let one_if_present get_feature_path state action =
    let feature_path = get_feature_path action in
    `run_after_reaction (fun () ->
      Which_features.these_features
        (if is_ok (find_feature state feature_path)
         then [ feature_path ]
         else []))
  ;;

  let one get_feature_path _ action =
    `run_after_reaction (fun () ->
      Which_features.these_features [ get_feature_path action ])
  ;;

  let rename ~to_:get_feature_path _ action =
    `run_after_reaction (fun () ->
      Which_features.Features [ { feature_path        = get_feature_path action
                                ; include_descendants = true
                                }])
  ;;

  let compress get_feature_path state action =
    let feature_path = get_feature_path action in
    let which_features =
      Which_features.these_features (list_of_iter (fun ~f ->
        Feature_forest.iter_children state.features feature_path ~f:(fun child ->
          f (Feature_path.compress_parent_exn (Feature.feature_path child)))))
    in
    `run_after_reaction (const which_features)
  ;;

  let all_review_managers_of_users get_users state action =
    let user_names = get_users action in
    let which_features =
      let these_features =
        let hset = Feature_path.Hash_set.create () in
        Set.iter user_names ~f:(fun user_name ->
          iter_review_managers_of_user state user_name ~f:(fun feature_id _ ->
            let feature = find_feature_by_id_exn state feature_id in
            Hash_set.add hset (Feature.feature_path feature)));
        hset
        |> Feature_path.Set.of_hash_set
        |> Set.to_list
      in
      Which_features.these_features these_features
    in
    `run_after_reaction (const which_features)
  ;;
end

let iter_which_features t (which_features : Which_features.t) ~f =
  match which_features with
  | All_features ->
    Feature_forest.iteri t.features ~f:(fun _feature_path feature -> f feature)
  | Features features ->
    List.iter features ~f:(fun { feature_path; include_descendants } ->
      if include_descendants
      then Feature_forest.iter_descendants t.features feature_path ~f
      else find_feature_exn t feature_path |> f)
;;

let invalidate_features t which_features =
  iter_which_features t which_features ~f:(fun feature ->
    Feature.invalidate_dependents feature);
;;

let check_cached_feature_attributes t which_features ~ignore_diffs_in_errors =
  let faulty_caches =
    alist_of_iter (fun ~f ->
      iter_which_features t which_features ~f:(fun feature ->
        match Feature.check_cached_attributes feature ~ignore_diffs_in_errors with
        | Ok () -> ()
        | Error err -> f (Feature.feature_path feature) err))
    |> List.sort ~cmp:(fun (f1, _) (f2, _) -> Feature_path.compare f1 f2)
  in
  if List.is_empty faulty_caches
  then Ok ()
  else Or_error.error "faulty caches" faulty_caches
         [%sexp_of: (Feature_path.t * Error.t) list]
;;

let crs_by_assignee t feature crs =
  let feature_owner = Feature.first_owner feature in
  let alternate_names = User_info.alternate_names t.user_info ~which:`All in
  List.map crs ~f:(fun cr ->
    let assignee =
      Cr_comment.Assignee.user_name (Cr_comment.assignee cr) ~feature_owner
        ~alternate_names
    in
    assignee, cr)
  |> User_name.Table.of_alist_multi
;;

let update_crs t feature crs_at_tip =
  let crs_by_assignee_at_tip = crs_by_assignee t feature crs_at_tip in
  let all_users_involved =
    (* We include all existing review managers because we need to clear their CRs if they
       previously had CRs but now do not. *)
    User_name.Set.union_list
      [ User_name.Set.of_list (Hashtbl.keys crs_by_assignee_at_tip)
      ; users_with_existing_review_manager t feature
      ]
  in
  Set.iter all_users_involved ~f:(fun user ->
    let cr_comments =
      Option.value (Hashtbl.find crs_by_assignee_at_tip user) ~default:[]
    in
    let base_facts = Feature.base_facts feature in
    let feature_id = Feature.feature_id feature in
    match Hashtbl2_pair.find t.review_managers feature_id user with
    | Some review_manager ->
      Review_manager.update_crs review_manager cr_comments ~base_facts
    | None ->
      ignore (create_review_manager t feature user ~cr_comments : Review_manager.t));
;;

let update_review_manager_goals t feature =
  let whole_feature_followers = Feature.whole_feature_followers feature in
  let whole_feature_reviewers = Feature.whole_feature_reviewers feature in
  let review_goal = Feature.review_goal feature in
  let all_users_involved =
    let may_reviewers =
      match Feature.diff_from_base_to_tip feature with
      | Known (Ok diff_from_base_to_tip) ->
        Diff2s.may_reviewers diff_from_base_to_tip ~include_file_followers:true
      | Pending_since _ | Known (Error _) ->
        User_name.Set.empty
    in
    User_name.Set.union_list
      [ may_reviewers
      ; whole_feature_followers
      ; whole_feature_reviewers
      (* We include all existing review managers because we need to clear them if the
         user has no CRs or review obligations at tip. *)
      ; users_with_existing_review_manager t feature
      ]
  in
  let feature_id = Feature.feature_id feature in
  Set.iter all_users_involved ~f:(fun user ->
    let review_manager =
      match Hashtbl2_pair.find t.review_managers feature_id user with
      | Some review_manager -> review_manager
      | None -> create_review_manager t feature user ~cr_comments:[]
    in
    match Feature.indexed_diff4s feature with
    | Pending_since _ -> ()
    | Known indexed_diff4s ->
      Review_manager.update_review_goal review_manager
        ~review_goal
        ~indexed_diff4s
        ~is_whole_feature_follower:(Set.mem whole_feature_followers user)
        ~is_whole_feature_reviewer:(Set.mem whole_feature_reviewers user)
  );
;;

let repartition_crs_by_assignee t feature =
  match Hashtbl2_pair.find1 t.review_managers (Feature.feature_id feature) with
  | None -> ()
  | Some review_managers_by_user_name ->
    with_return (fun ret ->
      let all_crs =
        Hashtbl.fold review_managers_by_user_name ~init:[]
          ~f:(fun ~key:_ ~data:review_manager acc ->
            match Review_manager.crs review_manager with
            | Ok crs -> crs @ acc
            | Error _exn ->
              (* If we see an error, then all review managers contains either no CRs or
                 an error.  So there's no point in repartitioning, and it would replace
                 the errors by simply no crs. *)
              ret.return ())
      in
      Feature.invalidate_dependents feature;
      update_crs t feature all_crs;
      (* because we might create review managers, we have to call this too *)
      update_review_manager_goals t feature)
;;

(* This takes about one second on prod state. *)
let repartition_crs_and_cr_soons_by_assignee_for_all_features t =
  Cr_soons.repartition t.cr_soons
    ~alternate_names:(User_info.alternate_names t.user_info ~which:`All);
  Hashtbl.iter t.features_by_id ~f:(fun feature ->
    repartition_crs_by_assignee t feature)
;;

let repartition_crs_and_cr_soons_by_assignee_if_needed t ~may_repartition_crs ~f =
  if not may_repartition_crs
  then f ()
  else (
    let before = User_info.alternate_names t.user_info ~which:`All in
    Exn.protect ~f
      ~finally:(fun () ->
        let after = User_info.alternate_names t.user_info ~which:`All in
        if User_name_by_alternate_name.compare before after <> 0
        then repartition_crs_and_cr_soons_by_assignee_for_all_features t))
;;

let change_fully_reviewed_revisions_internal t
      (query : Persist.Change_fully_reviewed_revisions_query.t) =
  match Query.action query with
  | Add    rev -> Hash_set.add    t.fully_reviewed_revisions rev
  | Remove rev -> Hash_set.remove t.fully_reviewed_revisions rev
;;

let change_fully_reviewed_revisions t query =
  change_fully_reviewed_revisions_internal t query;
  Serializer.append_to (serializer_exn t) query
    ~file:fully_reviewed_revisions_queries_file
    (module Persist.Change_fully_reviewed_revisions_query);
;;

let initialize_and_sync_file_system serializer =
  List.iter ~f:(fun dir -> Serializer.add_subtree serializer ~dir)
    [ archived_features_dir
    ; catch_up_dir
    ; event_subscriptions_dir
    ; features_dir
    ; dynamic_upgrade_dir
    ; push_events_dir
    ; unclean_workspaces_dir
    ; user_info_dir
    ; worker_cache_dir
    ];
  Serializer.prior_changes_synced_to_file_system serializer
;;

let prior_changes_synced_to_file_system t =
  Serializer.prior_changes_synced_to_file_system (serializer_exn t)
;;

let feature_deserializer ~dynamic_upgrade_state=
  let open Deserializer.Let_syntax in
  let%map_open () = return ()
  and feature = Feature.deserializer ~dynamic_upgrade_state
  and review_managers =
    in_subdir review_managers_dir_in_feature_dir
      (all_subdirs Review_manager.deserializer)
  in
  feature, review_managers
;;

let add_feature_exn t feature =
  let feature_id = Feature.feature_id feature in
  Feature_forest.add_exn t.features (Feature.feature_path feature) feature;
  Hashtbl.set t.features_by_id ~key:feature_id ~data:feature;
  set_feature_parties t feature;
  initialize_cached_feature_attributes t feature;
  Cached.Invalidator.set_callback (Feature.cache_invalidator feature)
    ~callback_no_exn:(fun () ->
      Feature_updates_manager.on_update t.feature_updates_manager feature);
  Feature_updates_manager.on_update t.feature_updates_manager feature;
  match Feature.cr_soons feature with
  | Pending_since _ | Known (Error _) -> ()
  | Known (Ok cr_soons) -> Cr_soons.update_feature t.cr_soons cr_soons
;;

let install_deserialized_feature t feature review_managers =
  add_feature_exn t feature;
  Map.iteri review_managers ~f:(fun ~key:user ~data:review_manager ->
    let user = User_name.of_string (File_name.to_string user) in
    let review_manager : Review_manager.t =
      review_manager
        ~whole_feature_followers:(Feature.whole_feature_followers feature)
        ~whole_feature_reviewers:(Feature.whole_feature_reviewers feature)
        ~users_using_locked_sessions:(User_info.Using_locked_sessions.get_set t.user_info)
        ~review_goal:(Feature.review_goal feature)
        ~indexed_diff4s:(Feature.indexed_diff4s feature)
        ~register_catch_up:(make_register_catch_up t feature user)
        ~feature_cache_invalidator:(Feature.cache_invalidator feature)
        ~dynamic_upgrade_state:t.dynamic_upgrade_state
    in
    add_review_manager t feature review_manager);
;;

let get_cached_or_reload_archived_feature_exn t feature_id =
  let open Async in
  match Archived_features.Cache.find t.archived_features feature_id with
  | Some feature_protocol -> return feature_protocol
  | None ->
    let archived_feature =
      ok_exn (Archived_features.find_by_id t.archived_features feature_id)
    in
    let archived_feature_dir = archived_feature_dir archived_feature in
    Serializer.uncompress_subtree_if_needed (serializer_exn t) ~dir:archived_feature_dir;
    (* After any of these binds, an other [get_cached_or_reload_archived_feature_exn] or
       [unarchive] could be running. Because [compress_subtree] and
       [uncompress_subtree_if_needed] are idempotent, it looks like the worst that can
       happen is that some of the calls to [get_cached_or_reload_archived_feature_exn]
       will fail. *)
    let%bind () = prior_changes_synced_to_file_system t in
    let%bind (feature, _) =
      Deserializer.load
        (feature_deserializer ~dynamic_upgrade_state:t.dynamic_upgrade_state)
        ~root_directory:(Abspath.append
                           (Serializer.root_directory (serializer_exn t))
                           archived_feature_dir)
        ~serializer:Serializer.do_nothing
    in
    let%map () =
      Serializer.compress_subtree_if_needed (serializer_exn t) ~dir:archived_feature_dir;
      prior_changes_synced_to_file_system t
    in
    (let line_count_by_user =
       Cached.create
         (module Line_count.Cached_in_feature.By_user)
         ~compute_depends_on:(fun () -> [])
         ~compute_result:(fun () -> User_name.Map.empty)
     in
     let next_steps =
       Cached.create
         (module Next_steps)
         ~compute_depends_on:(fun () -> [])
         ~compute_result:(fun () -> [])
     in
     let review_analysis =
       Cached.create
         (module Review_analysis_opt)
         ~compute_depends_on:(fun () -> [])
         ~compute_result:(fun () -> None)
     in
     Feature.set_cached_attributes feature
       ~line_count_by_user
       ~next_steps
       ~review_analysis);
    let feature_path = Feature.feature_path feature in
    let remote_repo_path =
      match find_feature t (Feature_path.root_path feature_path) with
      | Ok x -> remote_repo_path t x
      | Error _ -> Remote_repo_path.null
    in
    let feature_protocol =
      Feature.to_protocol feature
        ~remote_repo_path
        ~has_children:false
        ~cr_summary:
          (Or_error.error_string "CRs unavailable for archived features")
        ~line_count_by_user:
          (Or_error.error_string "Line counts unavailable for archived features")
        ~is_archived:(Archived_feature.to_is_archived archived_feature)
        ~is_rebased:true
        ~users_with_unclean_workspaces:(users_with_unclean_workspaces t feature_path)
    in
    let feature_protocol =
      match Iron_protocol.Feature.recover_diff_of_its_latest_release feature_protocol with
      | Some feature_protocol -> feature_protocol
      | None -> feature_protocol
    in
    Archived_features.Cache.add t.archived_features feature_protocol;
    feature_protocol
;;

let restrict_diff_from_base_to_tip
      t
      (feature_protocol : Iron_protocol.Feature.t)
      ~(what_diff       : What_diff.t)
  =
  let existing_feature = lazy (find_feature_exn t feature_protocol.feature_path) in
  let reviewer =
    match what_diff with
    | None
    | Whole_diff | Whole_diff_plus_ignored -> Reviewer.synthetic_whole_feature_reviewer
    | For user_name ->
      (match feature_protocol.is_archived with
       | Yes _ -> ()
       | No -> ignore (find_review_manager t (force existing_feature) user_name
                       |> ok_exn : Review_manager.t));
      Iron_protocol.Feature.reviewer_in_feature feature_protocol user_name
  in
  let diff_from_base_to_tip =
    let diff_from_base_to_tip = feature_protocol.diff_from_base_to_tip in
    match what_diff with
    | None -> Known (Or_error.error_string "no diff requested")
    | Whole_diff_plus_ignored -> diff_from_base_to_tip
    | Whole_diff | For _ ->
      Or_pending.Or_error.map diff_from_base_to_tip ~f:(fun diff2s ->
        Diff2s.restrict_to_may_review_or_follow diff2s reviewer)
  in
  reviewer, diff_from_base_to_tip
;;

let get_maybe_archived_feature_and_reviewer_exn t ~what_feature ~what_diff =
  let open Async in
  let { Maybe_archived_feature_spec. feature_spec; namespace } = what_feature in
  let sexp_of_archived_feature
        { Archived_feature.
          feature_id
        ; feature_path
        ; owners
        ; archived_at
        ; _ } =
    [%sexp
      [ { feature_id   : Feature_id.t }
      ; { feature_path : Feature_path.t
        ; owners       : User_name.t list
        ; archived_at  : Time.t
        }
      ]
    ]
  in
  let order_most_recently_archived_feature_first
        (f1 : Archived_feature.t)
        (f2 : Archived_feature.t)
    =
    Time.compare f2.archived_at f1.archived_at
  in
  let get_archived_feature_exn = function
    | [] -> failwith "no such archived feature"
    | [ feature ] -> feature
    | ( _::_::_ ) as features ->
      let features = List.sort features ~cmp:order_most_recently_archived_feature_first in
      raise_s
        [%sexp
          "multiple archived features matching"
        , (features : archived_feature list)
        ]
  in
  let get_cached_or_reload_archived_feature_exn archived_feature =
    get_cached_or_reload_archived_feature_exn t
      (Archived_feature.feature_id archived_feature)
  in
  let find archived_features existing =
    match namespace, archived_features, existing with
    | ( `All
      | `Existing
      | `Existing_or_most_recently_archived
      | `Existing_or_with_catch_up
      ), _, Some feature ->
      return (feature_to_protocol t feature)

    | `Existing_or_with_catch_up, (_::_), None ->
      List.filter archived_features ~f:(fun (archived_feature : Archived_feature.t) ->
        Hashtbl2_pair.mem1 t.catch_up_managers archived_feature.feature_path)
      |> get_archived_feature_exn
      |> get_cached_or_reload_archived_feature_exn

    | `Existing_or_most_recently_archived, (_::_), None ->
      List.min_elt archived_features ~cmp:order_most_recently_archived_feature_first
      |> Option.value_exn ~here:[%here]
      |> get_cached_or_reload_archived_feature_exn

    | `Archived, ((_::_) as features), (Some _ | None)
    | `All, ((_::_) as features), None ->
      get_cached_or_reload_archived_feature_exn
        (get_archived_feature_exn features)

    | _, [], None   ->
      raise_s
        [%sexp
          "no such feature"
        , (what_feature.feature_spec : Maybe_archived_feature_spec.Feature_spec.t)
        ]

    | `Archived, [], Some feature ->
      raise_s
        [%sexp
          "namespace mismatch, feature is active",
          { requested_feature = (what_feature : Maybe_archived_feature_spec.t)
          ; actual_namespace  = `Existing
          ; feature_path      = (Feature.feature_path feature : Feature_path.t)
          ; feature_id        = (Feature.feature_id   feature : Feature_id.t)
          }
        ]

    | `Existing, ((_::_) as features), None ->
      let err =
        match Or_error.try_with (fun () -> get_archived_feature_exn features) with
        | Ok feature ->
          Error.create "archived feature matching"
            feature [%sexp_of: archived_feature]
        | Error err  -> err
      in
      raise_s
        [%sexp
          [ "namespace mismatch, no such active feature"
          ; "requested_feature", (what_feature : Maybe_archived_feature_spec.t)
          ; "actual_namespace" , `Archived
          ; (err : Error.t)
          ]
        ]
  in
  let%map feature_protocol =
    match feature_spec with
    | `Feature_id feature_id ->
      let archived_features =
        match Archived_features.find_by_id t.archived_features feature_id with
        | Error _    -> []
        | Ok feature -> [ feature ]
      in
      let existing = Hashtbl.find t.features_by_id feature_id in
      find archived_features existing

    | `Feature_path feature_path ->
      let archived_features =
        Archived_features.find_by_path t.archived_features feature_path
      in
      let existing =
        match find_feature t feature_path with
        | Ok feature -> Some feature
        | Error _    -> None
      in
      find archived_features existing
  in
  let reviewer, diff_from_base_to_tip =
    restrict_diff_from_base_to_tip t feature_protocol ~what_diff
  in
  reviewer, { feature_protocol with diff_from_base_to_tip }
;;

let get_maybe_archived_feature_exn t what_feature =
  let open Async in
  let%map (_reviewer, feature_protocol) =
    get_maybe_archived_feature_and_reviewer_exn t ~what_feature ~what_diff:None
  in
  feature_protocol
;;

let assigned_release_or_rebase t feature =
  Assigned_release_or_rebase.create feature
    ~parent:(find_parent_exn t (Feature.feature_path feature))
;;

let execute_timed_event t id (action : Timed_event.Action.t) =
  match action with
  | Next_base_update_expiration { feature_id } ->
    (match find_feature_by_id t feature_id with
     | None -> ()
     | Some feature ->
       Feature.fire_next_base_update_expiration_if_applicable feature ~expiration_id:id)
;;

let deserializer = Deserializer.with_serializer (fun serializer ->
  let open Deserializer.Let_syntax in
  let%bind dynamic_upgrade_state =
    Deserializer.in_subdir dynamic_upgrade_dir Dynamic_upgrade.State.deserializer
  in
  let%map_open () = return ()
  and user_info = in_subdir user_info_dir User_info.deserializer
  and features_and_review_managers =
    in_subdir features_dir
      (all_subdirs (feature_deserializer ~dynamic_upgrade_state))
  and archived_features =
    in_subdir archived_features_dir
      (Archived_features.deserializer ~dynamic_upgrade_state)
  and catch_up_managers =
    in_subdir catch_up_dir
      (all_subdirs (all_subdirs Catch_up_session.deserializer))
  and fully_reviewed_edges =
    sequence_of (module Persist.Fully_reviewed_edges)
      ~in_file:fully_reviewed_edges_file
  and fully_reviewed_revisions =
    sequence_of (module Persist.Change_fully_reviewed_revisions_query)
      ~in_file:fully_reviewed_revisions_queries_file
  and fact_actions = sequence_of (module Fact.Action.Persist) ~in_file:fact_actions_file
  and push_events = in_subdir push_events_dir Push_events.deserializer
  and worker_cache = in_subdir worker_cache_dir Worker_cache.deserializer
  and unclean_workspaces =
    in_subdir unclean_workspaces_dir
      (Unclean_workspaces_manager.deserializer ~dynamic_upgrade_state)
  and event_subscriptions =
    in_subdir event_subscriptions_dir Event_subscriptions.deserializer
  in
  fun ~server_config ->
    let features = Feature_forest.create () in
    let feature_updates_manager =
      Feature_updates_manager.create
        ~children_of:(fun feature_path ->
          list_of_iter (Feature_forest.iter_children features feature_path))
        ()
    in
    let t =
      { features
      ; features_by_id            = Feature_id.Table.create ()
      ; cr_soons                  = Cr_soons.create ()
      ; archived_features
      ; features_by_parties       = Features_by_parties.create ()
      ; review_managers           = Review_managers.create ()
      ; catch_up_managers         = Catch_up_managers.create ()
      ; fully_reviewed_edges      = Fully_reviewed_edges.create ()
      ; fully_reviewed_revisions  = Fully_reviewed_revisions.create ()
      ; bookmarks_without_feature = Bookmarks_without_feature.create ()
      ; user_info
      ; fact_db                   = Fact.Db.create ()
      ; server_config
      ; serializer                = None
      ; timed_event_table         = Timed_event.the_table ()
      ; cached_attributes_errors  = Queue.create ()
      ; rpc_stats                 = Rpc_stats.create ()
      ; metrics                   = Metrics.create ()
      ; dynamic_upgrade_state
      ; push_events
      ; worker_cache
      ; unclean_workspaces
      ; event_subscriptions
      ; feature_updates_manager
      }
    in
    Timed_event.set_execute_exn ~execute:(execute_timed_event t);
    Map.iteri catch_up_managers ~f:(fun ~key:user_name ~data:catch_up_sessions ->
      let user_name = User_name.of_string (File_name.to_string user_name) in
      Map.iteri catch_up_sessions ~f:(fun ~key:_ ~data:catch_up_session ->
        let catch_up_session = catch_up_session ~user_name in
        let feature_path = Catch_up_session.feature_path catch_up_session in
        let catch_up_manager = find_catch_up_manager t feature_path user_name in
        Catch_up_manager.add catch_up_manager catch_up_session;
      ));
    (* We turn off stabilization until the end of deserialization to avoid wasting
       time eagerly computing intermediate values that will not be used.  This was
       a particular problem with CR-soons. *)
    Incr.set_should_stabilize false;
    List.iter fully_reviewed_edges ~f:(fun fully_reviewed_edge ->
      add_fully_reviewed_edge t fully_reviewed_edge);
    List.iter fully_reviewed_revisions ~f:(fun q ->
      change_fully_reviewed_revisions_internal t q);
    (* We process the features in increasing order of feature-path length, so
       that parents are always added before children. *)
    let features_and_review_managers =
      features_and_review_managers
      |> Map.data
      |> List.sort ~cmp:(fun (feature1, _) (feature2, _) ->
        Int.compare
          (Feature_path.num_parts (Feature.feature_path feature1))
          (Feature_path.num_parts (Feature.feature_path feature2)))
    in
    List.iter features_and_review_managers ~f:(fun (feature, review_managers) ->
      install_deserialized_feature t feature review_managers);
    List.iter fact_actions ~f:(fun action ->
      ok_exn (Fact.Db.handle_action t.fact_db action));
    Incr.set_should_stabilize true;
    t.serializer <- Some serializer;
    (* Repartition of the crs using the _current_ aliases.  Because we potentially
       create review managers, we need [t.serializer] to contain the real
       serializer. *)
    repartition_crs_and_cr_soons_by_assignee_for_all_features t;
    t
)
;;

let rpc_implementations = ref []

module Eager_deferred_infix = struct
  let (>>|) deferred f =
    let open Async in
    match Deferred.peek deferred with
    | Some d -> return (f d)
    | None -> deferred >>| f
  ;;

  let (>>=) deferred f =
    let open Async in
    match Deferred.peek deferred with
    | Some d -> f d
    | None -> deferred >>= f
  ;;

  let _ = (>>|)
  let _ = (>>=)
end

let implement_deferred_rpc
      (type action) (type reaction)
      ?override_sexp_of_action
      ?override_sexp_of_reaction
      ~log
      (check_cached_in_features : action Post_check_in_features.t)
      m
      f =
  let module M = (val m : Iron_protocol.Iron_versioned_rpc.S
                  with type action   = action
                   and type reaction = reaction)
  in
  let implementations =
    M.implement_deferred_rpc (fun state ~version query ->
      let query_uuid = Query.uuid query in
      let server_received_query_at = Time.now () in
      let open Async in
      let open Eager_deferred_infix in
      Monitor.try_with ~run:`Now (fun () ->
        let `run_after_reaction checked_features =
          Post_check_in_features.which_features check_cached_in_features state
            (Query.action query)
        in
        f state query
        >>| fun reaction ->
        let checked_features = checked_features () in
        (match
           check_cached_feature_attributes state checked_features
             ~ignore_diffs_in_errors:true
         with
         | Ok () -> ()
         | Error err ->
           let err =
             let rpc_name = M.name in
             Error.create_s
               [%sexp
                 "faulty cached attributes post-RPC check",
                 { rpc_name         : string
                 ; query            : M.action Query.t
                 ; reaction         : M.reaction
                 ; checked_features : Which_features.t
                 ; err              : Error.t
                 }
               ]
           in
           if am_functional_testing
           && is_none (Sys.getenv "IRON_FUNCTIONAL_TESTING_CACHED_ATTRIBUTES_ERRORS")
           then Error.raise err;
           Log.Global.sexp ~level:`Error
             ~tags:[ "faulty cached attributes", "query" ]
             [%sexp (err : Error.t)];
           if Queue.length state.cached_attributes_errors >= 500
           then ignore (Queue.dequeue_exn state.cached_attributes_errors : Error.t);
           Queue.enqueue state.cached_attributes_errors err);
        reaction)
      >>| fun reaction ->
      let reaction =
        match reaction with
        | Ok _ as r -> r
        | Error exn ->
          error "error" (M.name, Monitor.extract_exn exn) [%sexp_of: string * exn]
      in
      let reaction =
        if not am_functional_testing
        then reaction
        else (
          match invariant state with
          | () -> reaction
          | exception post_condition_exn ->
            error_s
              [%sexp
                "post condition failed after rpc",
                { post_condition_exn : Exn.t
                ; reaction           : M.reaction Or_error.t
                }
              ])
      in
      let response = Response.create reaction ~query_uuid ~server_received_query_at in
      Rpc_stats.add_hit state.rpc_stats ~took:(Response.server_took response)
        { by          = Query.by query
        ; rpc_name    = M.name
        ; rpc_version = version
        };
      Feature_updates_manager.broadcast_updates state.feature_updates_manager;
      if log
      then (
        let sexp_of_query =
          match override_sexp_of_action with
          | None -> M.sexp_of_query
          | Some sexp_of_action -> [%sexp_of: action Query.t]
        in
        let sexp_of_response =
          match override_sexp_of_reaction with
          | None -> M.sexp_of_response
          | Some sexp_of_reaction -> [%sexp_of: reaction Or_error.t Response.t]
        in
        Log.Global.sexp ~level:`Info ~tags:[ "what", "query" ]
          [%sexp
            ((M.name, query, response)
             : string * query * response)]);
      response)
  in
  rpc_implementations := implementations @ !rpc_implementations;
;;

let implement_rpc
      ?override_sexp_of_action ?override_sexp_of_reaction
      ~log changes_features m f =
  implement_deferred_rpc ?override_sexp_of_action ?override_sexp_of_reaction
    ~log changes_features m
    (fun state query -> Async.return (f state query));
;;

let implement_deferred_pipe_rpc
      (type action) (type reaction)
      ?override_sexp_of_action
      ~log
      m
      f =
  let module M = (val m : Iron_protocol.Iron_versioned_rpc.S_pipe_rpc
                  with type action   = action
                   and type reaction = reaction)
  in
  let implementations =
    M.implement_deferred_rpc (fun state ~version query ->
      let server_received_query_at = Time.now () in
      let open Async in
      let open Eager_deferred_infix in
      let subscription_or_error =
        Event_subscriptions.add state.event_subscriptions
          ~rpc_name:M.name
          ~rpc_version:version query
          ~sexp_of_action:(Option.value override_sexp_of_action ~default:M.sexp_of_action)
      in
      (match subscription_or_error with
       | Error _ as error -> return error
       | Ok subscription ->
         Monitor.try_with_or_error ~extract_exn:true (fun () -> f state query)
         >>| function
         | Error _ as e ->
           Event_subscriptions.remove state.event_subscriptions subscription;
           e
         | Ok reader' ->
           let reader' =
             Pipe.map reader' ~f:(fun value ->
               Event_subscriptions.Subscription.tick subscription;
               value)
           in
           let reader, writer = Pipe.create () in
           don't_wait_for
             (Pipe.transfer_id reader' writer >>| fun () -> Pipe.close writer);
           don't_wait_for
             (Event_subscriptions.Subscription.closed subscription >>| function
              | `Removed -> ()
              | `Dropped dropped ->
                if not (Pipe.is_closed writer)
                then (
                  Pipe.write_without_pushback writer (Error dropped);
                  Pipe.close writer)
             );
           don't_wait_for
             (Pipe.closed reader >>| fun () ->
              Event_subscriptions.remove state.event_subscriptions subscription
             );
           Ok reader)
      >>| fun reaction ->
      let took = Time.diff (Time.now ()) server_received_query_at in
      Rpc_stats.add_hit state.rpc_stats ~took
        { by          = Query.by query
        ; rpc_name    = M.name
        ; rpc_version = version
        };
      (if log
       then (
         let sexp_of_query =
           match override_sexp_of_action with
           | None -> M.sexp_of_query
           | Some sexp_of_action -> [%sexp_of: action Query.t]
         in
         Log.Global.sexp ~level:`Info ~tags:[ "what", "query-pipe" ]
           [%sexp ((M.name, query) : string * query)]));
      match reaction with
      | Ok _ as ok -> ok
      | Error err  -> error "error" (M.name, err) [%sexp_of: string * Error.t]
    )
  in
  rpc_implementations := implementations @ !rpc_implementations
;;

let implement_pipe_rpc ?override_sexp_of_action ~log m f =
  implement_deferred_pipe_rpc ?override_sexp_of_action ~log m
    (fun state query -> Async.return (f state query))
;;

let only_server_user_is_authorized_exn query =
  let query_user = Query.by query in
  let authorized_user = User_name.unix_login in
  if not (User_name.equal query_user authorized_user)
  then
    raise_s [%sexp "Unauthorized rpc for user"
                 , { query_user      : User_name.t
                   ; authorized_user : User_name.t
                   }]
;;

let users_with_admin_privileges t =
  Set.add (User_info.Admins.get_set t.user_info) User_name.unix_login
;;

let user_has_admin_privileges t user =
  User_name.equal user User_name.unix_login
  || User_info.Admins.mem t.user_info user
;;

let only_user_with_admin_privileges_is_authorized_exn t query =
  let by = Query.by query in
  if not (user_has_admin_privileges t by)
  then
    raise_s
      [%sexp
        "unauthorized RPC by user -- admin privileges required"
      , { user                        = (by                            : User_name.t)
        ; users_with_admin_privileges = (users_with_admin_privileges t : User_name.Set.t)
        }
      ]
;;

let check_catch_up_for_exn t ~for_ ~by =
  if User_info.are_acting_for_themselves_or_for_invalid_user t.user_info ~for_ ~by
  || (user_has_admin_privileges t by && not am_functional_testing)
  || (am_functional_testing
      && Option.is_none (Sys.getenv "IRON_FUNCTIONAL_TESTING_CATCH_UP"))
  then ()
  else
    raise_s
      [%sexp
        "unauthorized attempt to modify someone else's catch-up review",
        { for_         = (for_ : User_name.t)
        ; requested_by = (by   : User_name.t)
        }
      ]
;;

let review_authorization_may_skip_user_exn t feature review_manager query
      ~reason ~create_catch_up_for_me ~is_reviewing_for
  =
  let review_authorization =
    Review_manager.Review_authorization.create
      review_manager
      ~allow_review_for:(Ref.Permissioned.get (Feature.allow_review_for feature))
      ~are_acting_for_themselves_or_for_invalid_user:
        (User_info.are_acting_for_themselves_or_for_invalid_user t.user_info)
      ~current_feature_goal_subset:
        (what_goal_subset_needs_to_be_reviewed feature)
      ~reason
      ~create_catch_up_for_me
      query
    |> ok_exn
  in
  match
    Review_manager.Review_authorization.unauthorized_for_user_with_only_follow_lines
      review_authorization
  with
  | Ok () -> `Ok review_authorization
  | Error err ->
    match is_reviewing_for with
    | `User _ ->
      if user_has_admin_privileges t (Query.by query)
      && not am_functional_testing
      then `Ok review_authorization
      else `Error err
    | `All_users | `All_users_but _ ->
      (* This variant allow one to skip the followers when running -for all, for
         convenience (regardless of whether the user has admin privileges). *)
      `Unauthorized_for_users_with_only_follow_lines err
;;

let review_authorization_exn t feature review_manager query
      ~reason ~create_catch_up_for_me =
  match
    review_authorization_may_skip_user_exn t feature review_manager query
      ~reason ~create_catch_up_for_me ~is_reviewing_for:(`User ())
  with
  | `Ok review_authorization -> review_authorization
  | `Error e | `Unauthorized_for_users_with_only_follow_lines e -> Error.raise e
;;

let () =
  let module Remove_alternate_names = Iron_protocol.Remove_alternate_names in
  implement_rpc ~log:true Post_check_in_features.all
    (module Remove_alternate_names)
    (fun t query ->
       only_user_with_admin_privileges_is_authorized_exn t query;
       let { Remove_alternate_names.Action. alternate_names; which; may_repartition_crs } =
         Query.action query
       in
       repartition_crs_and_cr_soons_by_assignee_if_needed t ~may_repartition_crs
         ~f:(fun () ->
           User_info.remove_alternate_names_exn t.user_info alternate_names ~which))
;;

let () =
  implement_rpc ~log:false Post_check_in_features.none
    (module Iron_protocol.Get_rpc_stats)
    (fun t query ->
       let () = Query.action query in
       Rpc_stats.to_protocol t.rpc_stats)
;;

let () =
  let module Update_valid_users_and_aliases =
    Iron_protocol.Update_valid_users_and_aliases
  in
  implement_rpc ~log:true
    (* No post check here because it is not immediate to know what features to check and
       we already have an admin RPC to check all features periodically. *)
    Post_check_in_features.none
    (module Update_valid_users_and_aliases)
    (fun t query ->
       only_user_with_admin_privileges_is_authorized_exn t query;
       let { Update_valid_users_and_aliases.Action.
             valid_users_and_aliases
           ; may_repartition_crs
           }
         = Query.action query
       in
       repartition_crs_and_cr_soons_by_assignee_if_needed t ~may_repartition_crs
         ~f:(fun () ->
           User_info.update_valid_users_and_aliases_exn t.user_info
             valid_users_and_aliases))
;;

let () =
  let module User_set = Iron_protocol.User_set in
  implement_rpc ~log:true
    Post_check_in_features.none
    (module User_set.Get)
    (fun t query ->
       let module M =
         (val (User_info.get_user_set (Query.action query)) : User_info.User_set.S)
       in
       M.get_set t.user_info
    );
  implement_rpc ~log:true
    (Post_check_in_features.all_review_managers_of_users
       User_set.Change.Action.user_names)
    (module User_set.Change)
    (fun t query ->
       let { User_set.Change.Action.
             user_set
           ; user_names
           ; change
           ; idempotent
           } = Query.action query
       in
       let () =
         let require_admin_privileges =
           match user_set with
           | `Admins -> true
           | `Feeding_metrics -> true
           | `Using_locked_sessions ->
             Set.exists user_names ~f:(fun user_name ->
               not (User_name.equal user_name (Query.by query)))
         in
         if require_admin_privileges
         then only_user_with_admin_privileges_is_authorized_exn t query
       in
       let module M = (val (User_info.get_user_set user_set) : User_info.User_set.S) in
       let () =
         (match change with
          | `Add    -> M.add    t.user_info user_names ~idempotent
          | `Remove -> M.remove t.user_info user_names ~idempotent)
         |> ok_exn
       in
       match user_set with
       | `Admins | `Feeding_metrics -> () (* Nothing special to update *)
       | `Using_locked_sessions ->
         let is_using_locked_sessions =
           match change with
           | `Add    -> true
           | `Remove -> false
         in
         Set.iter user_names ~f:(fun user_name ->
           iter_review_managers_of_user t user_name ~f:(fun _ review_manager ->
             Review_manager.set_is_using_locked_sessions review_manager
               is_using_locked_sessions)))
;;

let occurrences_by_user_name t =
  let table : User_name_occurrence.t list User_name.Table.t = User_name.Table.create () in
  let add occurrence user_name =
    Hashtbl.update table user_name ~f:(fun occurrences ->
      let occurrences = Option.value occurrences ~default:[] in
      if List.mem occurrences occurrence ~equal:User_name_occurrence.equal
      then occurrences
      else occurrence :: occurrences)
  in
  let process f field = f (Field.get field t) in
  Fields.iter
    ~archived_features:ignore
    ~bookmarks_without_feature:
      (process
         (Hashtbl2_pair.iter ~f:(fun _ user_name _ ->
            add Has_bookmark_without_feature user_name)))
    ~catch_up_managers:
      (process
         (Hashtbl2_pair.iter ~f:(fun _ user_name catch_up_manager ->
            if Line_count.Catch_up.total
                 (Catch_up_manager.line_count_remaining_to_catch_up catch_up_manager)
               > 0
            then add Catch_up_reviewer user_name)))
    ~cr_soons:
      (* All the people with _inactive_ CR-soons; we don't care about the active ones. *)
      (process Cr_soons.iter_inactive_assignees ~f:(add Cr_assignee))
    ~fact_db:ignore
    ~features:ignore
    ~features_by_id:
      (process
         (Hashtbl.iteri ~f:(fun ~key:_ ~data:feature ->
            List.iter (Feature.owners feature) ~f:(add Owner);
            Set.iter (Feature.whole_feature_followers feature) ~f:(fun user ->
              add Whole_feature_follower user);
            Set.iter (Feature.whole_feature_reviewers feature) ~f:(fun user ->
              add Whole_feature_reviewer user;
              if seconding_is_recommended t ~for_:user feature ~even_if_locked:false
              then add Recommended_seconder user);
            Option.iter (Feature.seconder feature) ~f:(add Seconder))))
    ~features_by_parties:ignore
    ~fully_reviewed_edges:ignore
    ~fully_reviewed_revisions:ignore
    ~review_managers:
      (process
         (Hashtbl2_pair.iter ~f:(fun _ user_name review_manager ->
            if Review_manager.can_make_progress review_manager Entire_goal
            then add Reviewer user_name;
            match Review_manager.crs review_manager with
            | Error _ | Ok [] -> ()
            | Ok (_ :: _) -> add Cr_assignee user_name)))
    ~serializer:ignore
    ~timed_event_table:ignore
    ~cached_attributes_errors:ignore
    ~rpc_stats:ignore
    ~metrics:ignore
    ~dynamic_upgrade_state:ignore
    ~push_events:ignore
    ~worker_cache:ignore
    ~unclean_workspaces:(process (fun t ->
      Set.iter (Unclean_workspaces_manager.users t) ~f:(add Has_unclean_workspaces)))
    ~server_config:ignore
    ~user_info:ignore
    ~event_subscriptions:ignore
    ~feature_updates_manager:ignore;
  table
;;

let () =
  let module Refresh_existing_users = Iron_protocol.Refresh_existing_users in
  implement_rpc ~log:true Post_check_in_features.none
    (module Refresh_existing_users)
    (fun t query ->
       only_user_with_admin_privileges_is_authorized_exn t query;
       let () = Query.action query in
       User_info.refresh_existing_users t.user_info
         ~occurrences_by_user_name:(occurrences_by_user_name t))
;;

let () =
  let module Repartition_crs = Iron_protocol.Repartition_crs in
  implement_rpc ~log:true
    Post_check_in_features.none
    (* [repartition_crs_by_assignee] handles invalidation, if needed. *)
    (module Repartition_crs)
    (fun t query ->
       only_user_with_admin_privileges_is_authorized_exn t query;
       let () = Query.action query in
       repartition_crs_and_cr_soons_by_assignee_for_all_features t)
;;

let () =
  let module Define_typos = Iron_protocol.Define_typos in
  implement_rpc ~log:true
    (* [repartition_crs_by_assignee] handles invalidation, if needed. *)
    Post_check_in_features.none
    (module Define_typos)
    (fun t query ->
       only_user_with_admin_privileges_is_authorized_exn t query;
       let { Define_typos.Action. definitions; may_repartition_crs } =
         Query.action query
       in
       let user_names = List.map definitions ~f:(fun d -> d.means) in
       User_info.ensure_users_exist t.user_info user_names;
       repartition_crs_and_cr_soons_by_assignee_if_needed t ~may_repartition_crs
         ~f:(fun () -> User_info.define_typos_exn t.user_info definitions))
;;

let () =
  let module Get_feature_email_recipients = Iron_protocol.Get_feature_email_recipients in
  implement_rpc ~log:false Post_check_in_features.none
    (module Get_feature_email_recipients)
    (fun t query ->
       let { Get_feature_email_recipients.Action. feature_path; sent_upon } =
         Query.action query
       in
       { recipients = feature_email (find_feature_exn t feature_path) ~sent_upon })
;;

let () =
  let module Get_invalid_users = Iron_protocol.Get_invalid_users in
  implement_rpc ~log:false Post_check_in_features.none
    (module Get_invalid_users)
    (fun t query ->
       let () = Query.action query in
       User_info.invalid_users t.user_info)
;;

let create_fully_reviewed_edge rev_zero feature ~even_if_release_is_locked =
  let feature_path = Feature.feature_path feature in
  Or_error.try_with (fun () ->
    let is_fully_reviewed =
      match Feature.next_steps feature with
      | [ Unlock Release ] ->
        even_if_release_is_locked
        || raise_s [%sexp
             (concat [ "release is locked -- consider using "
                     ; Switch.even_if_release_is_locked
                     ] : string)
           , (feature_path : Feature_path.t)]
      | next_steps ->
        List.exists next_steps ~f:(function
          | Release
          | Wait_for_continuous_release -> true
          | _ -> false)
    in
    if not is_fully_reviewed
    then
      raise_s [%sexp "feature is not fully reviewed", (feature_path : Feature_path.t)];
    { Fully_reviewed_edge.
      rev_zero
    ; from_    = Feature.base feature
    ; to_      = Feature.tip feature
    ; reason   = Fully_reviewed_edge.Reason.Review
    })
;;

let () =
  let module Add_fully_reviewed_edge = Iron_protocol.Add_fully_reviewed_edge in
  implement_rpc ~log:true
    (* Adding an edge may indeed affect a feature, but only indirectly, via an
       [update_bookmark], which will handle the cached attributes invalidation. *)
    Post_check_in_features.none
    (module Add_fully_reviewed_edge)
    (fun t query ->
       let { Add_fully_reviewed_edge.Action. rev_zero; edge } = Query.action query in
       let edge_opt =
         match edge with
         | `From_to (from_, to_) ->
           only_user_with_admin_privileges_is_authorized_exn t query;
           Some { Fully_reviewed_edge.
                  rev_zero
                ; from_
                ; to_
                ; reason   = External (Query.with_action query ())
                }
         | `Feature_base_to_tip { feature_path
                                ; even_if_release_is_locked
                                ; if_feature_is_empty
                                } ->
           let feature = find_feature_exn t feature_path in
           if Rev.equal_node_hash (Feature.base feature) (Feature.tip feature)
           then (
             match if_feature_is_empty with
             | `Do_nothing -> None
             | `Fail ->
               failwithf
                 "cannot add empty fully reviewed edges.  Consider using %s"
                 Switch.do_nothing_if_feature_is_empty ())
           else Some (ok_exn (create_fully_reviewed_edge
                                rev_zero feature ~even_if_release_is_locked))
       in
       match edge_opt with
       | None -> ()
       | Some edge -> add_fully_reviewed_edge t edge)
;;

let ensure_can_archive feature ~for_ ~must_be_owner =
  if must_be_owner && not (Feature.is_owner feature for_)
  then
    raise_s
      [%sexp
        "only an owner of a feature can archive it",
        { owners        = (Feature.owners feature : User_name.t list)
        ; requested_for = (for_ : User_name.t)
        }
      ];
  if Feature.is_permanent feature
  then failwith "cannot archive a permanent feature";
;;

let archive_feature t query feature ~for_ ~must_be_owner ~reason_for_archiving =
  let feature_path = Feature.feature_path feature in
  ensure_can_archive feature ~must_be_owner ~for_;
  if Feature_forest.has_children_exn t.features (Feature.feature_path feature)
  then failwith "cannot archive a feature that has children";
  let archived_feature =
    Archived_feature.create feature ~archived_at:(Query.at query)
      ~reason_for_archiving
  in
  Feature_updates_manager.on_archive t.feature_updates_manager feature;
  Feature_forest.remove_exn t.features feature_path;
  Hashtbl.remove t.features_by_id (Feature.feature_id feature);
  Features_by_parties.remove_feature t.features_by_parties (Feature.feature_id feature);
  Cr_soons.remove_feature t.cr_soons feature_path;
  Hashtbl2_pair.remove_all1 t.review_managers (Feature.feature_id feature);
  (if Feature_path.is_root feature_path
   then (
     match Feature.remote_repo_path feature with
     | None -> ()
     | Some remote_repo_path ->
       Hashtbl2_pair.remove_all1 t.bookmarks_without_feature remote_repo_path));
  Push_events.change t.push_events (Clear_features [ Feature.feature_id feature ]);
  let archived_feature_dir = archived_feature_dir archived_feature in
  Serializer.rename (serializer_exn t)
    ~from_:(feature_dir (Feature.feature_id feature))
    ~to_:archived_feature_dir;
  Serializer.compress_subtree_if_needed (serializer_exn t) ~dir:archived_feature_dir;
  Archived_features.add t.archived_features query archived_feature;
;;

let () =
  let module Archive_feature = Iron_protocol.Archive_feature in
  implement_rpc ~log:true
    Post_check_in_features.none
    (module Archive_feature)
    (fun t query ->
       let { Archive_feature.Action.
             feature_path; rev_zero; for_; reason_for_archiving
           } = Query.action query
       in
       let feature = find_feature_exn t feature_path in
       let remote_repo_path = remote_repo_path t feature ~rev_zero in
       archive_feature t query feature ~for_ ~must_be_owner:true ~reason_for_archiving;
       let send_email_to =
         Email_address.Set.union_list
           [ feature_email feature ~sent_upon:Archive
           ; [ Query.by query; for_ ]
             |> List.map ~f:Email_address.of_user_name
             |> Email_address.Set.of_list
           ]
       in
       { Archive_feature.Reaction. remote_repo_path; send_email_to })
;;

let () =
  let module Brain_forget = Iron_protocol.Brain_forget in
  implement_rpc ~log:true (Post_check_in_features.one Brain_forget.Action.feature_path)
    (module Brain_forget)
    (fun t query ->
       let { Brain_forget.Action. feature_path; for_; what_to_forget } =
         Query.action query
       in
       let feature = find_feature_exn t feature_path in
       let review_manager = ok_exn (find_review_manager t feature for_) in
       let review_authorization =
         review_authorization_exn t feature review_manager query
           ~reason:`Not_supported ~create_catch_up_for_me:false
       in
       Review_manager.forget_from_brain_exn review_manager review_authorization
         ~what_to_forget;
       if User_name.equal for_ (Query.by query)
       then (
         (* Rationale: When a user with catch-up forgets file in their brain, they are
            going to have to review them again, so there is no need to catch-up on those
            files.  The actual catch-up diff is more likely to be stale than helpful.
            However, we do not want users to be able to silently delete others' people
            catch-up. *)
         match Hashtbl2_pair.find t.catch_up_managers feature_path for_ with
         | None -> ()
         | Some catch_up_manager ->
           let should_catch_up =
             match what_to_forget with
             | `All -> const true
             | `Files files -> Hash_set.mem (Path_in_repo.Hash_set.of_list files)
           in
           let catch_up_sessions =
             Catch_up_manager.find_all_for_feature_id
               catch_up_manager (Feature.feature_id feature)
           in
           List.iter catch_up_sessions ~f:(fun catch_up_session ->
             let ids_to_catch_up =
               List.filter_map (Catch_up_session.diff4s_to_catch_up catch_up_session)
                 ~f:(fun diff4_to_catch_up ->
                   if should_catch_up
                        (Diff4_to_catch_up.path_in_repo_at_f2 diff4_to_catch_up)
                   then Some (Diff4_to_catch_up.id diff4_to_catch_up)
                   else None)
             in
             catch_up_in_session t query feature_path catch_up_manager catch_up_session
               ids_to_catch_up for_)));
;;

let () =
  let module Clear_bookmarks_without_feature =
    Iron_protocol.Clear_bookmarks_without_feature in
  implement_rpc ~log:true Post_check_in_features.none
    (module Clear_bookmarks_without_feature)
    (fun t query ->
       only_user_with_admin_privileges_is_authorized_exn t query;
       let { Clear_bookmarks_without_feature.Action.
             remote_repo_path
           } = Query.action query
       in
       Hashtbl2_pair.remove_all1 t.bookmarks_without_feature remote_repo_path)
;;

let change_feature t feature query
      (updates : Iron_protocol.Change_feature.Update.t list) =
  let users =
    User_name.Set.union_list
      (List.map updates ~f:(function
         | `Add_inheritable_owners us
         | `Add_owners us
         | `Set_inheritable_owners us
         | `Set_owners us
           -> User_name.Set.of_list us
         | `Add_inheritable_whole_feature_reviewers us
         | `Add_inheritable_whole_feature_followers us
         | `Add_reviewing us
         | `Add_whole_feature_followers us
         | `Add_whole_feature_reviewers us
         | `Remove_inheritable_owners us
         | `Remove_inheritable_whole_feature_followers us
         | `Remove_inheritable_whole_feature_reviewers us
         | `Remove_owners us
         | `Remove_reviewing us
         | `Remove_whole_feature_followers us
         | `Remove_whole_feature_reviewers us
         | `Set_inheritable_whole_feature_followers us
         | `Set_inheritable_whole_feature_reviewers us
         | `Set_reviewing (`Only us)
         | `Set_reviewing (`All_but us)
         | `Set_whole_feature_followers us
         | `Set_whole_feature_reviewers us
           -> us
         | `Add_inheritable_send_email_to _
         | `Add_inheritable_send_email_upon _
         | `Add_send_email_to _
         | `Add_send_email_upon _
         | `Remove_inheritable_properties _
         | `Remove_properties _
         | `Remove_send_email_to _
         | `Remove_send_email_upon _
         | `Remove_inheritable_send_email_to _
         | `Remove_inheritable_send_email_upon _
         | `Set_base _
         | `Set_crs_are_enabled _
         | `Set_crs_shown_in_todo_only_for_users_reviewing _
         | `Set_description _
         | `Set_inheritable_properties _
         | `Set_inheritable_release_process _
         | `Set_inheritable_who_can_release_into_me _
         | `Set_inheritable_crs_shown_in_todo_only_for_users_reviewing _
         | `Set_inheritable_xcrs_shown_in_todo_only_for_users_reviewing _
         | `Set_inheritable_send_email_to _
         | `Set_inheritable_send_email_upon _
         | `Set_is_permanent _
         | `Set_lines_required_to_separate_ddiff_hunks _
         | `Set_properties _
         | `Set_release_process _
         | `Set_review_is_enabled _
         | `Set_reviewing (`All | `Whole_feature_reviewers)
         | `Set_send_email_to _
         | `Set_send_email_upon _
         | `Set_who_can_release_into_me _
         | `Set_xcrs_shown_in_todo_only_for_users_reviewing _
           -> User_name.Set.empty))
  in
  User_info.ensure_users_exist t.user_info (Set.to_list users);
  (* We filter out invalid updates and don't feed them to [Feature.change]. *)
  let updates, errors =
    let errors_ref = ref [] in
    let updates_ref = ref [] in
    List.iter updates ~f:(fun update ->
      let ok_or_error =
        Or_error.try_with (fun () ->
          match update with
          | `Set_base base ->
            if not (Rev.equal_node_hash base (Feature.base feature))
            then ok_exn (Feature_locks.check_unlocked (Feature.locks feature) Rebase)
          | _ -> ())
      in
      match ok_or_error with
      | Ok ()       -> updates_ref := update                :: !updates_ref
      | Error error -> errors_ref  := (update, Error error) :: !errors_ref);
    List.rev !updates_ref
  , List.rev !errors_ref
  in
  let whole_feature_followers = Feature.whole_feature_followers feature in
  let whole_feature_reviewers = Feature.whole_feature_reviewers feature in
  let owner_for_crs = Feature.first_owner feature in
  let owners = Feature.owners feature in
  let result = Feature.change feature query updates in
  let owners_changed =
    not (Set.equal
           (owners |> User_name.Set.of_list)
           (Feature.owners feature |> User_name.Set.of_list))
  in
  let whole_feature_reviewers_changed =
    not (Set.equal whole_feature_reviewers (Feature.whole_feature_reviewers feature))
  in
  let whole_feature_followers_changed =
    not (Set.equal whole_feature_followers (Feature.whole_feature_followers feature))
  in
  (if List.exists Features_by_parties.Parties.all ~f:(function
     | Owners -> owners_changed
     | Whole_feature_followers -> whole_feature_followers_changed)
   then set_feature_parties t feature);
  (if whole_feature_reviewers_changed
   || whole_feature_followers_changed
   then update_review_manager_goals t feature);
  (if not (User_name.equal owner_for_crs (Feature.first_owner feature))
   then repartition_crs_by_assignee t feature);
  result @ errors
;;

let change_feature_exn t feature query updates =
  List.iter (change_feature t feature query updates)
    ~f:(fun (_, or_error) -> ok_exn or_error);
;;

let () =
  let module Change_feature = Iron_protocol.Change_feature in
  implement_rpc ~log:true (Post_check_in_features.one Change_feature.Action.feature_path)
    (module Change_feature)
    (fun t query ->
       let { Change_feature.Action. feature_path; updates } = Query.action query in
       let feature = find_feature_exn t feature_path in
       let requires_admin_privileges =
         List.exists updates ~f:(function
           | `Set_lines_required_to_separate_ddiff_hunks _ -> true
           | _ -> false)
       in
       (if requires_admin_privileges
        then only_user_with_admin_privileges_is_authorized_exn t query);
       change_feature t feature query updates)
;;

let () =
  let module Expect_next_base_update = Iron_protocol.Expect_next_base_update in
  implement_rpc ~log:true
    (Post_check_in_features.one Expect_next_base_update.Action.feature_path)
    (module Expect_next_base_update)
    (fun t query ->
       let { Expect_next_base_update.Action.
             feature_path
           ; for_
           ; expected_base
           } = Query.action query in
       let feature = find_feature_exn t feature_path in
       Feature.expect_next_base_update_exn feature ~for_ expected_base)
;;

let () =
  let module Change_fully_reviewed_revisions =
    Iron_protocol.Change_fully_reviewed_revisions
  in
  implement_rpc ~log:true Post_check_in_features.none
    (module Change_fully_reviewed_revisions)
    (fun t query ->
       let { Change_fully_reviewed_revisions.Action. what_to_do } =
         Query.action query
       in
       only_user_with_admin_privileges_is_authorized_exn t query;
       let query = Query.with_action query what_to_do in
       change_fully_reviewed_revisions t query)
;;

let () =
  let module Check_cached_feature_attributes =
    Iron_protocol.Check_cached_feature_attributes
  in
  implement_rpc ~log:false Post_check_in_features.none
    (module Check_cached_feature_attributes)
    (fun t query ->
       only_user_with_admin_privileges_is_authorized_exn t query;
       let { Check_cached_feature_attributes.Action.
             which_features; ignore_diffs_in_errors
           }
         = Query.action query
       in
       ok_exn (check_cached_feature_attributes t which_features ~ignore_diffs_in_errors))
;;

let () =
  let module With_cached_attributes_errors =
    Iron_protocol.With_cached_attributes_errors
  in
  implement_rpc ~log:false Post_check_in_features.none
    (module With_cached_attributes_errors)
    (fun t query ->
       match Query.action query with
       | Get ->
         let errors = Queue.to_list t.cached_attributes_errors in
         if not (List.is_empty errors)
         then Error.raise (Error.of_list errors)
       | Clear ->
         only_user_with_admin_privileges_is_authorized_exn t query;
         Queue.clear t.cached_attributes_errors)
;;

let () =
  let module With_timed_event_errors =
    Iron_protocol.With_timed_event_errors
  in
  implement_rpc ~log:false Post_check_in_features.none
    (module With_timed_event_errors)
    (fun t query ->
       match Query.action query with
       | Get ->
         let errors = Timed_event.Table.Errors.get t.timed_event_table in
         if not (List.is_empty errors)
         then Error.raise (Error.of_list errors)
       | Clear ->
         only_user_with_admin_privileges_is_authorized_exn t query;
         Timed_event.Table.Errors.clear t.timed_event_table)
;;

let need_diff4s_starting_from t feature =
  let users_by_review_edge =
    alist_of_iter (iter_review_managers_of_feature t feature)
    |> List.concat_map
         ~f:(fun (user, review_manager) ->
           List.map (Set.to_list
                       (Review_manager.need_diff4s_starting_from review_manager))
             ~f:(fun review_edge -> (review_edge, user)))
    |> Review_edge.Table.of_alist_multi
  in
  (match Feature.review_goal feature with
   | Error _ -> ()
   | Ok goal ->
     (* If a review manager is created during a bookmark update, it could
        have a session going to the end of the review goal, so we need to
        ask for any diff4s that would bring from there to the next review
        goal. *)
     let edge = Review_goal.review_edge goal in
     if not (Hashtbl.mem users_by_review_edge edge)
     then Hashtbl.add_exn users_by_review_edge ~key:edge ~data:[]);
  List.map (Hashtbl.to_alist users_by_review_edge) ~f:(fun (edge, users) ->
    edge, User_name.Set.of_list users)
;;

let worker_cache_feature_revs t ?need_diff4s_starting_from:need_diff4s feature =
  let need_diff4s_starting_from =
    match need_diff4s with
    | Some already_computed -> already_computed
    | None -> need_diff4s_starting_from t feature
  in
  let diff4s_revs =
    List.concat_map need_diff4s_starting_from
      ~f:(fun ({ Review_edge. base; tip }, _users) -> [ base; tip ])
    |> Rev.Compare_by_hash.Set.of_list
  in
  { Worker_cache.Feature_revs.
    diff4s_revs
  ; base = Feature.base feature
  ; tip  = Feature.tip  feature
  }
;;

let () =
  let module With_worker_cache = Iron_protocol.With_worker_cache in
  implement_rpc ~log:true Post_check_in_features.none
    (module With_worker_cache)
    (fun t query ->
       only_user_with_admin_privileges_is_authorized_exn t query;
       match Query.action query with
       | Clear_features clear ->
         (match clear with
          | All_features  -> Worker_cache.clear t.worker_cache `All
          | which_features ->
            iter_which_features t which_features ~f:(fun feature ->
              let feature_revs = worker_cache_feature_revs t feature in
              Worker_cache.clear t.worker_cache (`Feature_revs feature_revs)))
       | Clear_revs revs -> Worker_cache.clear t.worker_cache (`Revs revs)
       | Set_max_size max_size ->
         Worker_cache.set_max_size t.worker_cache ~max_size
       | Set_status status ->
         Worker_cache.set_status t.worker_cache ~status
       | Set_max_items_per_rpc max_items_per_rpc ->
         Worker_cache.set_max_items_per_rpc t.worker_cache ~max_items_per_rpc)
;;

let () =
  let module With_archived_features_cache = Iron_protocol.With_archived_features_cache in
  implement_rpc ~log:true Post_check_in_features.none
    (module With_archived_features_cache)
    (fun t query ->
       only_user_with_admin_privileges_is_authorized_exn t query;
       match Query.action query with
       | Clear what_to_clear ->
         (match what_to_clear with
          | `All           -> Archived_features.Cache.clear  t.archived_features
          | `Feature_id id -> Archived_features.Cache.remove t.archived_features id)
       | Set_max_size max_size ->
         Archived_features.set_max_cache_size t.archived_features query ~max_size)
;;

let () =
  let module With_unclean_workspaces = Iron_protocol.With_unclean_workspaces in
  implement_rpc ~log:true Post_check_in_features.none
    (module With_unclean_workspaces)
    (fun t query ->
       let is_authorized_exn user =
         if not am_functional_testing
         && not (User_name.equal user (Query.by query))
         then only_user_with_admin_privileges_is_authorized_exn t query
       in
       match Query.action query with
       | Remove_user user ->
         is_authorized_exn user;
         Unclean_workspaces_manager.remove_user_exn t.unclean_workspaces query user
       | Remove_machine (user, machine) ->
         is_authorized_exn user;
         Unclean_workspaces_manager.remove_machine_exn t.unclean_workspaces
           query user machine)
;;

let check_workspace_update_exn query ~for_ =
  let requested_by = Query.by query in
  if not (am_functional_testing || User_name.equal requested_by for_)
  then
    raise_s
      [%sexp
        "unauthorized workspace update with [-for]",
        { requested_by  = (requested_by : User_name.t)
        ; requested_for = (for_         : User_name.t)
        }
      ]
;;

let () =
  let module With_dynamic_upgrade_state = Iron_protocol.With_dynamic_upgrade_state in
  implement_rpc ~log:true Post_check_in_features.none
    (module With_dynamic_upgrade_state)
    (fun t query ->
       only_user_with_admin_privileges_is_authorized_exn t query;
       match Query.action query with
       | Set value -> Dynamic_upgrade.set_exn t.dynamic_upgrade_state value)
;;

let () =
  let module Update_unclean_workspaces = Iron_protocol.Update_unclean_workspaces in
  implement_rpc ~log:true Post_check_in_features.none
    (module Update_unclean_workspaces)
    (fun t query ->
       let action = Query.action query in
       check_workspace_update_exn query ~for_:action.for_;
       Unclean_workspaces_manager.update t.unclean_workspaces query action)
;;

let () =
  let module Check_invariant = Iron_protocol.Check_invariant in
  implement_rpc ~log:true Post_check_in_features.none
    (module Check_invariant)
    (fun t query ->
       only_server_user_is_authorized_exn query;
       let () = Query.action query in
       invariant t)
;;

let () =
  let module Complete = Iron_protocol.Complete in
  implement_rpc ~log:false Post_check_in_features.none
    (module Complete)
    (fun t query ->
       let { Complete.Action. types; prefix } = Query.action query in
       let complete_from_roots iter_root =
         let completions = ref [] in
         let maybe string =
           if String.is_prefix ~prefix string then completions := string :: !completions
         in
         Feature_forest.iteri_roots t.features ~f:(iter_root ~f:maybe);
         !completions
       in
       let relative_feature_path_completion_types, other_completions =
         List.partition_map types ~f:(function
           | Feature_path          -> `Fst `Existing
           | Feature_path_with_catch_up -> `Fst `With_catch_up
           | Absolute_feature_path ->
             `Snd (Feature_forest.complete t.features ~prefix `Of_full_name)
           | Archived_feature_path ->
             `Fst `Archived
           | Metric_name ->
             `Snd (Metrics.complete t.metrics ~prefix)
           | Remote_repo_path ->
             `Snd (complete_from_roots (fun ~f:maybe _ feature ->
               match Feature.remote_repo_path feature with
               | None -> ()
               | Some remote_repo_path ->
                 maybe (Remote_repo_path.to_string remote_repo_path)))
           | Root_feature_path ->
             `Snd (complete_from_roots (fun ~f:maybe feature_name _ ->
               maybe (Feature_name.to_string feature_name)))
           | User_info which ->
             `Snd (User_info.complete t.user_info ~prefix which))
       in
       let relative_feature_path_completions =
         Feature_path.complete ~prefix `Of_partial_name
           ~iter_features:(fun ~f ->
             let f_key key _data = f key in
             List.iter relative_feature_path_completion_types ~f:(function
               | `Existing -> Feature_forest.iteri t.features ~f:f_key
               | `With_catch_up -> Hashtbl2_pair.iter1 t.catch_up_managers ~f:f_key
               | `Archived -> Archived_features.iteri t.archived_features ~f:f_key
             ))
       in
       String.Set.stable_dedup_list
         (List.concat (relative_feature_path_completions :: other_completions))
    )
;;

let%test_unit _ =
  let inherit_attributes_and_properties_from_parent t query feature ~parent =
    Batch_of_feature_changes.empty
    |> Batch_of_feature_changes.add_inherited_from_parent
         ~parent_inheritable_attributes:(Feature.inheritable_attributes parent)
    |> Batch_of_feature_changes.to_feature_updates feature
    |> change_feature_exn t feature query
  in
  let (_ : t -> _ Query.t -> Feature.t -> parent:Feature.t -> unit) =
    inherit_attributes_and_properties_from_parent
  in
  ()
;;

let create_feature_exn t query =
  let { Iron_protocol.Create_feature.Action.
        feature_path
      ; owners
      ; is_permanent
      ; description
      ; base
      ; tip
      ; add_whole_feature_reviewers
      ; reviewing
      ; rev_zero
      ; remote_repo_path            = remote_repo_path_opt
      ; allow_non_cr_clean_base
      ; properties
      } =
    Query.action query
  in
  User_info.ensure_users_exist t.user_info
    (owners @ Set.to_list add_whole_feature_reviewers);
  let base =
    match find_parent t feature_path with
    | Error _   -> Option.value base ~default:rev_zero
    | Ok parent ->
      (* check that we're in the right repo *)
      ignore (remote_repo_path t parent ~rev_zero : Remote_repo_path.t);
      ok_exn (Feature_locks.check_unlocked (Feature.locks parent) Create_child);
      match base with
      | Some rev -> rev
      | None ->
        let base = Feature.tip parent in
        Or_pending.iter (Feature.tip_facts parent) ~f:(fun facts ->
          match
            Rev_facts.check_true facts base
              ~allow_non_cr_clean:allow_non_cr_clean_base
              ~error_msg_if_not_cr_clean:
                (concat [ "revision is not CR clean -- consider using "
                        ; Switch.allow_non_cr_clean_base
                        ])
          with
          | Ok () -> ()
          | Error err ->
            raise_s
              [%sexp (sprintf !"invalid base revision %{Rev#hum}" base : string)
                   , (err : Error.t)
              ]
        );
        base
  in
  ok_exn (Feature_forest.check_add t.features feature_path);
  let tip = Option.value tip ~default:base in
  let feature_id = Feature_id.create () in
  (* Extra attributes requested by [create] shall be set after the inheritance logic has
     run below, so as to chose what the right behavior is for conflicting attributes.  See
     [add_whole_feature_reviewers] for an example. *)
  let feature =
    Feature.create_exn query
      ~feature_id
      ~feature_path ~owners ~is_permanent ~description ~base ~tip ~rev_zero
      ~remote_repo_path:remote_repo_path_opt
      ~dynamic_upgrade_state:t.dynamic_upgrade_state
      ~serializer:(lazy (create_feature_serializer t feature_id))
  in
  add_feature_exn t feature;
  Serializer.add_subtree (serializer_exn t) ~dir:(review_managers_dir feature_id);
  let batch_of_feature_changes =
    let changes = Batch_of_feature_changes.empty in
    let changes =
      match find_parent t feature_path with
      | Error _   -> changes
      | Ok parent ->
        Batch_of_feature_changes.add_inherited_from_parent changes
          ~parent_inheritable_attributes:(Feature.inheritable_attributes parent)
    in
    let changes =
      match properties with
      | None            -> changes
      | Some properties -> Batch_of_feature_changes.add_properties changes ~properties
    in
    Batch_of_feature_changes.add_whole_feature_reviewers changes
      ~whole_feature_reviewers:add_whole_feature_reviewers
  in
  let set_reviewing =
    match reviewing with
    | `Whole_feature_reviewers -> []
    | `First_owner ->
      [ `Set_reviewing (`Only (User_name.Set.singleton (List.hd_exn owners))) ]
  in
  change_feature_exn t feature query
    (Batch_of_feature_changes.to_feature_updates feature batch_of_feature_changes
     @ set_reviewing);
  feature
;;

let () =
  let module Create_feature = Iron_protocol.Create_feature in
  implement_rpc ~log:true Post_check_in_features.none
    (module Create_feature)
    (fun t query ->
       let feature = create_feature_exn t query in
       { Create_feature.Reaction.
         feature_id       = Feature.feature_id feature
       ; remote_repo_path = remote_repo_path t feature
       ; tip              = Feature.tip feature
       }
    )
;;

module Rename = struct
  type t =
    { from_feature : Feature.t
    ; to_          : Feature_path.t
    }

  let to_hg_rename { from_feature; to_ } =
    { Iron_hg.Rename.
      feature_id = Feature.feature_id   from_feature
    ; from       = Feature.feature_path from_feature
    ; to_
    }
  ;;
end

let renames_exn t ~from_feature ~to_ ~to_may_exist ~check_rename_lock =
  let from = Feature.feature_path from_feature in
  ok_exn (Feature_path.check_renameable ~from ~to_);
  if not to_may_exist && is_ok (find_feature t to_)
  then raise_s [%sexp "cannot rename to an existing feature", (to_ : Feature_path.t)];
  if is_error (find_parent t to_)
  then raise_s [%sexp "the parent of the target feature has to exist"
                    , (to_ : Feature_path.t)];
  let from_features =
    from_feature :: ok_exn (Feature_forest.strict_descendants t.features
                              (Feature.feature_path from_feature))
  in
  (if check_rename_lock
   then
     from_features
     |> List.map ~f:(fun feature ->
       Feature_locks.check_unlocked (Feature.locks feature) Rename)
     |> Or_error.combine_errors_unit
     |> ok_exn);
  (* If we've reached here, the renames are OK. *)
  let to_parts = Feature_path.parts to_ in
  let from_num_parts = Feature_path.num_parts from in
  List.map from_features ~f:(fun from_feature ->
    let to_ =
      Feature_path.of_parts_exn
        (to_parts
         @ List.drop (Feature_path.parts (Feature.feature_path from_feature))
             from_num_parts)
    in
    { Rename. from_feature; to_ })
;;

let rename_features ?(between_removes_and_adds = ignore) t query renames =
  (* We are not updating bookmarks_without_feature, so it will be potentially out of sync,
     until the next synchronize-state rpc.  Notice that the make_register_catch_up
     callback closes over the feature and not the feature_path. *)
  let renames_in_increasing_order_of_num_parts =
    List.sort renames ~cmp:(fun (r1 : Rename.t) r2 ->
      Int.compare
        (Feature_path.num_parts r1.to_)
        (Feature_path.num_parts r2.to_))
  in
  (* Remove all features from the feature forest in decreasing order of num_parts, and
     then put them back in increasing order.  This ensures that the forest is valid at
     each step.  We do renaming after [between_removes_and_adds], because when this is
     being used for compress, that is what archives the feature being compressed.  This
     avoids problems if one of the features is being renamed to the feature that was
     archived. *)
  List.iter (List.rev renames_in_increasing_order_of_num_parts)
    ~f:(fun { from_feature; _ } ->
      let from = Feature.feature_path from_feature in
      Feature_forest.remove_exn t.features from);
  between_removes_and_adds ();
  List.iter renames_in_increasing_order_of_num_parts ~f:(fun { from_feature; to_ } ->
    (* The [on_rename] needs to happen before the feature changes feature_path.  The new
       path gets notified by the call to [Feature.invalidate_dependents] *)
    Feature_updates_manager.on_rename t.feature_updates_manager from_feature;
    let from = Feature.feature_path from_feature in
    Feature_forest.add_exn t.features to_ from_feature;
    rename_catch_up_sessions t query ~from_feature ~to_;
    Feature.rename_non_root from_feature query ~to_;
    Cr_soons.rename_non_root_feature t.cr_soons ~from ~to_;
    Feature.invalidate_dependents from_feature);
;;

let prepare_to_compress t feature ~for_ ~check_rename_lock =
  let feature_path = Feature.feature_path feature in
  if not (Rev.equal_node_hash (Feature.base feature) (Feature.tip feature))
  then failwith "cannot compress a feature whose base and tip are unequal";
  let parent =
    match find_parent t feature_path with
    | Ok parent -> parent
    | Error _ -> failwith "cannot compress a root feature"
  in
  ensure_can_archive feature ~must_be_owner:true ~for_;
  let feature_has_review_remaining =
    Map.exists (Feature.line_count_by_user feature |> ok_exn) ~f:(fun line_count ->
      let review =
        Line_count.Review.to_review_column_shown
          (To_goal_via_session.fully_known_exn line_count.review)
          ~have_potentially_blocking_review_session_in_progress:
            line_count.have_potentially_blocking_review_session_in_progress
      in
      Review_or_commit.count review > 0)
  in
  if feature_has_review_remaining
  then failwith "cannot compress feature that has review to be done";
  let all_renames = ref [] in
  Feature_forest.iter_children t.features feature_path ~f:(fun child ->
    let compressed_child_path =
      Feature_path.compress_parent_exn (Feature.feature_path child)
    in
    all_renames :=
      renames_exn t ~from_feature:child ~to_:compressed_child_path
        ~to_may_exist:(Feature_path.equal compressed_child_path feature_path)
        ~check_rename_lock
      @ !all_renames);
  parent, !all_renames
;;

let () =
  let module Prepare_to_compress = Iron_protocol.Prepare_to_compress in
  implement_rpc ~log:false Post_check_in_features.none
    (module Prepare_to_compress)
    (fun t query ->
       let { Prepare_to_compress.Action. feature_path; for_; rev_zero } =
         Query.action query
       in
       let feature = find_feature_exn t feature_path in
       let remote_repo_path = remote_repo_path t feature ~rev_zero in
       let parent, renames =
         prepare_to_compress t feature ~for_ ~check_rename_lock:true
       in
       { feature_tip      = Feature.tip feature
       ; parent_tip       = Feature.tip parent
       ; renames          = List.map renames ~f:Rename.to_hg_rename
       ; remote_repo_path
       })
;;

let () =
  let module Compress = Iron_protocol.Compress in
  implement_rpc ~log:false (Post_check_in_features.compress Compress.Action.feature_path)
    (module Compress)
    (fun t query ->
       let { Compress.Action. feature_path; for_ } = Query.action query in
       let feature = find_feature_exn t feature_path in
       let _, renames = prepare_to_compress t feature ~for_ ~check_rename_lock:false in
       rename_features t query renames ~between_removes_and_adds:(fun () ->
         archive_feature t query feature ~for_
           (* [must_be_owner:true] is already checked in [prepare_to_compress] and thus
              cannot fail here. *)
           ~must_be_owner:true ~reason_for_archiving:"compressed"))
;;

let () =
  let module Prepare_to_rename = Iron_protocol.Prepare_to_rename in
  implement_rpc ~log:false Post_check_in_features.none
    (module Prepare_to_rename)
    (fun t query ->
       let { Prepare_to_rename.Action.
             from
           ; to_
           ; rev_zero
           } = Query.action query in
       let from_feature = find_feature_exn t from in
       let remote_repo_path = remote_repo_path t from_feature ~rev_zero in
       let renames =
         renames_exn t ~from_feature ~to_ ~to_may_exist:false ~check_rename_lock:true
         |> List.map ~f:Rename.to_hg_rename
       in
       let to_parent = ok_exn (find_parent t to_) in
       { Prepare_to_rename.Reaction.
         from_feature_path      = from
       ; from_base              = Feature.base from_feature
       ; from_tip               = Feature.tip  from_feature
       ; to_parent_feature_path = Feature.feature_path to_parent
       ; to_parent_tip          = Feature.tip          to_parent
       ; renames
       ; remote_repo_path
       })
;;

let () =
  let module Rename_feature = Iron_protocol.Rename_feature in
  implement_rpc ~log:true (Post_check_in_features.rename ~to_:Rename_feature.Action.to_)
    (module Rename_feature)
    (fun t query ->
       let { Rename_feature.Action. from; to_ } = Query.action query in
       let from_feature = ok_exn (find_feature t from) in
       rename_features t query
         (renames_exn t ~from_feature ~to_ ~to_may_exist:false ~check_rename_lock:false))
;;

let () =
  let module Copy_feature = Iron_protocol.Copy_feature in
  implement_rpc ~log:true Post_check_in_features.none (module Copy_feature)
    (fun t query ->
       let { Iron_protocol.Copy_feature.Action.
             from_; to_; rev_zero; without_copying_review
           } = Query.action query
       in
       let feature = find_feature_exn t from_ in
       match find_feature t to_ with
       | Ok _ ->
         raise_s [%sexp "feature that is being copied to already exists"
                      , (to_ : Feature_path.t)]
       | Error _ ->
         let from_parent = ok_exn (find_parent t from_) in
         let to_parent   = ok_exn (find_parent t to_)   in
         if not (Feature_id.equal
                   (Feature.feature_id from_parent) (Feature.feature_id to_parent))
         then failwith "parents must be the same to copy.  Consider using copy then rename.";
         if Feature_forest.has_children_exn t.features from_
         then raise_s [%sexp "feature has children", (from_ : Feature_path.t)];
         (match
            without_copying_review,
            create_fully_reviewed_edge rev_zero feature ~even_if_release_is_locked:true
          with
          | true , Error _ -> ()
          | false, Ok edge -> add_fully_reviewed_edge t edge
          | true , Ok _ ->
            failwith (concat [ "cannot use "
                             ; Switch.without_copying_review
                             ; " because the feature is fully reviewed, and thus \
                                review will be copied"
                             ]);
          | false, Error _ ->
            let who_has_done_some_review =
              list_of_iter (fun ~f ->
                iter_review_managers_of_feature t feature
                  ~f:(fun user review_manager ->
                    if Review_manager.have_done_some_review review_manager
                    then f user))
            in
            if not (List.is_empty who_has_done_some_review)
            then (
              let who_has_done_some_review =
                List.sort who_has_done_some_review ~cmp:User_name.compare
              in
              raise_s
                [%sexp
                  (concat ["feature is not fully reviewed, review has been done and \
                            will not be copied -- consider using "
                          ; Switch.without_copying_review
                          ] : string),
                  { who_has_done_some_review : User_name.t list
                  }
                ]);
         );
         let create_query =
           Query.with_action query
             { Iron_protocol.Create_feature.Action.
               feature_path                = to_
             ; owners                      = Feature.owners feature
             ; is_permanent                = Feature.is_permanent feature
             ; description                 = Feature.description feature
             ; base                        = Some (Feature.base feature)
             ; tip                         = Some (Feature.tip feature)
             ; add_whole_feature_reviewers = Feature.whole_feature_reviewers feature
             ; reviewing                   = `First_owner
             ; rev_zero
             ; remote_repo_path            = Feature.remote_repo_path feature
             ; allow_non_cr_clean_base     = true
             ; properties                  = Some (Feature.properties feature)
             }
         in
         let copy = create_feature_exn t create_query in
         Feature.set_review_is_enabled copy query
           (Feature.review_is_enabled feature);
         Feature.set_crs_are_enabled copy query
           (Feature.crs_are_enabled feature);
         Feature.set_crs_shown_in_todo_only_for_users_reviewing copy query
           (Feature.crs_shown_in_todo_only_for_users_reviewing feature);
         Feature.set_xcrs_shown_in_todo_only_for_users_reviewing copy query
           (Feature.xcrs_shown_in_todo_only_for_users_reviewing feature);
         { Copy_feature.Reaction.
           feature_id       = Feature.feature_id feature
         ; remote_repo_path = remote_repo_path t feature
         ; tip              = Feature.tip feature
         })
;;

let check_user_is_enabled_exn feature user_name =
  let feature_path = Feature.feature_path feature in
  if not (Feature.review_is_enabled feature)
  then raise_s [%sexp "review is not enabled for", (feature_path : Feature_path.t)];
  if not (Feature.user_is_currently_reviewing feature user_name)
  then raise_s [%sexp "user is not reviewing", (user_name : User_name.t)];
;;

let () =
  let module Forget_session = Iron_protocol.Session.Forget in
  implement_rpc ~log:true
    (Post_check_in_features.one Forget_session.Action.feature_path)
    (module Forget_session)
    (fun t query ->
       let { Forget_session.Action. feature_path; for_; review_session_id
           ; what_to_forget }
         = Query.action query
       in
       let feature = find_feature_exn t feature_path in
       let review_manager = ok_exn (find_review_manager t feature for_) in
       let review_authorization =
         review_authorization_exn t feature review_manager query
           ~reason:`Not_supported ~create_catch_up_for_me:false
       in
       Review_manager.forget_from_current_session_exn
         review_manager review_authorization query review_session_id ~what_to_forget)
;;

let () =
  let module Commit_session = Iron_protocol.Session.Commit in
  implement_rpc ~log:true
    (Post_check_in_features.one Commit_session.Action.feature_path)
    (module Commit_session)
    (fun t query ->
       let { Commit_session.Action. feature_path; for_; review_session_id }
         = Query.action query
       in
       let feature = find_feature_exn t feature_path in
       let review_manager = ok_exn (find_review_manager t feature for_) in
       let review_authorization =
         review_authorization_exn t feature review_manager query
           ~reason:`Not_supported ~create_catch_up_for_me:false
       in
       Review_manager.commit_current_session_exn
         review_manager review_authorization review_session_id)
;;

let () =
  let module Session_set_lock = Iron_protocol.Session.Set_lock in
  implement_rpc ~log:true
    (Post_check_in_features.one Session_set_lock.Action.feature_path)
    (module Session_set_lock)
    (fun t query ->
       let { Session_set_lock.Action.
             feature_path
           ; for_
           ; which_session
           ; set_is_locked_to
           }
         = Query.action query
       in
       let feature = find_feature_exn t feature_path in
       let review_manager = ok_exn (find_review_manager t feature for_) in
       let review_authorization =
         review_authorization_exn t feature review_manager query
           ~reason:`Not_supported ~create_catch_up_for_me:false
       in
       Review_manager.set_session_is_locked_exn
         review_manager review_authorization query ~which_session set_is_locked_to)
;;

let () =
  let module De_alias_feature = Iron_protocol.De_alias_feature in
  implement_rpc ~log:true
    (Post_check_in_features.one De_alias_feature.Action.feature_path)
    (module De_alias_feature)
    (fun t query ->
       let { De_alias_feature.Action. feature_path } = Query.action query in
       let user_name_by_alias = User_info.alternate_names t.user_info ~which:`Aliases in
       let de_aliased = ref [] in
       let did_not_de_alias_due_to_review_session_in_progress = ref [] in
       let nothing_to_do = ref [] in
       iter_review_managers_of_feature t (find_feature_exn t feature_path)
         ~f:(fun user review_manager ->
           let r =
             match Review_manager.de_alias_brain review_manager user_name_by_alias with
             | `De_aliased    -> de_aliased
             | `Nothing_to_do -> nothing_to_do
             | `Did_not_de_alias_due_to_review_session_in_progress ->
               did_not_de_alias_due_to_review_session_in_progress
           in
           r := user :: !r);
       { de_aliased
         = User_name.Set.of_list !de_aliased
       ; did_not_de_alias_due_to_review_session_in_progress
         = User_name.Set.of_list !did_not_de_alias_due_to_review_session_in_progress
       ; nothing_to_do
         = User_name.Set.of_list !nothing_to_do
       })
;;

let () =
  let module Feature_exists = Iron_protocol.Feature_exists in
  implement_rpc ~log:false Post_check_in_features.none
    (module Feature_exists)
    (fun t query ->
       let feature_path = Query.action query in
       match find_feature t feature_path with
       | Error _    -> No
       | Ok feature -> Yes (Feature.feature_id feature))
;;

let () =
  let module Get_brain = Iron_protocol.Get_brain in
  implement_rpc ~log:false Post_check_in_features.none
    (module Get_brain)
    (fun t query ->
       let { Get_brain.Action. feature_path; for_ } = Query.action query in
       let feature = find_feature_exn t feature_path in
       let review_manager = ok_exn (find_review_manager t feature for_) in
       let brain = Review_manager.brain review_manager in
       { Get_brain.Reaction.
         reviewer       = Review_manager.reviewer review_manager
       ; brain
       ; remote_rev_zero  = Feature.rev_zero feature
       ; remote_repo_path = remote_repo_path t feature
       })
;;

let () =
  let module Get_lines_required_to_separate_ddiff_hunks =
    Iron_protocol.Get_lines_required_to_separate_ddiff_hunks in
  implement_rpc ~log:false Post_check_in_features.none
    (module Get_lines_required_to_separate_ddiff_hunks)
    (fun t query ->
       let () = Query.action query in
       List.Assoc.map (Feature_forest.roots t.features) ~f:(fun feature ->
         Option.value ~default:Constants.lines_required_to_separate_ddiff_hunks_default
           (Feature.lines_required_to_separate_ddiff_hunks feature))
       |> Feature_name.Map.of_alist_exn)
;;

let () =
  let module Get_review_session = Iron_protocol.Get_review_session in
  implement_rpc ~log:true Post_check_in_features.none
    (module Get_review_session)
    (fun t query ->
       let { Get_review_session.Action.
             feature_path
           ; for_
           ; rev_zero
           ; ensure_reviewing
           ; which_session
           ; lock_session
           } =
         Query.action query
       in
       let feature = find_feature_exn t feature_path in
       let remote_repo_path = remote_repo_path t feature ?rev_zero in
       if ensure_reviewing then check_user_is_enabled_exn feature for_;
       let review_manager = ok_exn (find_review_manager t feature for_) in
       let may_be_reviewed_by =
         Allow_review_for.may_be_reviewed_by ~reviewed_for:for_
           (Ref.Permissioned.get (Feature.allow_review_for feature))
       in
       let review_session =
         Review_manager.get_session_exn review_manager
           (what_goal_subset_needs_to_be_reviewed feature)
           ~may_be_reviewed_by
           ~lines_required_to_separate_ddiff_hunks:
             (lines_required_to_separate_ddiff_hunks t feature_path
                ~fail_if_root_does_not_exist:true)
           ~which_session
       in
       let status =
         match review_session with
         | `Up_to_date ->
           (match Feature.next_bookmark_update feature with
            | Update_expected_since _ -> `Bookmark_update_is_pending
            | No_update_expected | No_update_expected_due_to_iron_bug _ ->
              (* We report [Up_to_date] even when there is an Iron bug, because the
                 feature owner is already informed about the Iron bug, and we don't want
                 to confuse reviewers into thinking there is something they should do. *)
              `Up_to_date)
         | `Review_session _ as result ->
           let should_lock_session =
             match lock_session with
             | `No -> false
             | `If_applicable ->
               User_name.equal for_ (Query.by query)
               && User_info.Using_locked_sessions.mem t.user_info for_
           in
           (if should_lock_session
            then (
              let review_authorization =
                review_authorization_exn t feature review_manager query
                  ~reason:`Not_supported ~create_catch_up_for_me:false
              in
              Review_manager.set_session_is_locked_exn
                review_manager review_authorization query ~which_session true));
           result
       in
       let may_second =
         seconding_is_recommended t ~for_ feature ~even_if_locked:false
       in
       { Get_review_session.Reaction.
         status
       ; feature_tip      = Feature.tip feature
       ; remote_rev_zero  = Feature.rev_zero feature
       ; remote_repo_path
       ; may_second
       })
;;

let () =
  let module Clear_catch_up_sessions = Iron_protocol.Clear_catch_up_sessions in
  implement_rpc ~log:true
    (Post_check_in_features.one_if_present Clear_catch_up_sessions.Action.feature_path)
    (module Clear_catch_up_sessions)
    (fun t query ->
       let { Clear_catch_up_sessions.Action.
             feature_path
           ; for_
           ; ok_if_nothing_cleared
           ; only_those_reviewed_by
           } = Query.action query
       in
       User_info.ensure_user_exists t.user_info for_;
       check_catch_up_for_exn t ~for_ ~by:(Query.by query);
       let user_name_by_alias = User_info.alternate_names t.user_info ~which:`Aliases in
       let de_alias unresolved_name =
         let user =
           User_name_by_alternate_name.to_user_name user_name_by_alias
             unresolved_name
         in
         User_info.ensure_user_exists t.user_info user;
         user
       in
       let only_those_reviewed_by = Blang.map only_those_reviewed_by ~f:de_alias in
       let should_clear diff4_to_catch_up =
         match Catch_up_session.Diff4_to_catch_up.kind diff4_to_catch_up with
         | Create_catch_up_for_me
         | Follower
         | Unfinished_review ->
           (match only_those_reviewed_by with
            | True -> true
            | _    -> false)
         | Reviewed_by_someone_else { reviewed_by; reason = _ } ->
           let reviewed_by =
             reviewed_by
             |> User_name.to_unresolved_name
             |> de_alias
           in
           Blang.eval only_those_reviewed_by (User_name.equal reviewed_by)
       in
       match Hashtbl2_pair.find t.catch_up_managers feature_path for_ with
       | None ->
         if is_error (Feature_forest.find t.features feature_path)
         && not (Archived_features.mem_feature_path t.archived_features feature_path)
         then raise_s [%sexp "unknown feature", (feature_path : Feature_path.t)]
       | Some catch_up_manager ->
         let catch_up_sessions =
           Catch_up_manager.find_all_for_feature_path catch_up_manager feature_path
         in
         let cleared_some = ref false in
         List.iter catch_up_sessions ~f:(fun catch_up_session ->
           let ids_to_clear =
             List.filter_map (Catch_up_session.diff4s_to_catch_up catch_up_session)
               ~f:(fun diff4_to_catch_up ->
                 if should_clear diff4_to_catch_up
                 then Some (Diff4_to_catch_up.id diff4_to_catch_up)
                 else None)
           in
           if not (List.is_empty ids_to_clear)
           then (
             cleared_some := true;
             catch_up_in_session t query feature_path catch_up_manager catch_up_session
               ids_to_clear for_));
         if not (!cleared_some || ok_if_nothing_cleared)
         then
           raise_s
             [%sexp
               "no catch-up to clear",
               { feature_path           : Feature_path.t
               ; for_                   : User_name.t
               ; only_those_reviewed_by : User_name.t Blang.t
               }
             ])
;;

let () =
  let module Get_catch_up_session = Iron_protocol.Get_catch_up_session in
  implement_rpc ~log:true Post_check_in_features.none
    (module Get_catch_up_session)
    (fun t query ->
       let { Get_catch_up_session.Action. feature_path; for_ } = Query.action query in
       match Hashtbl2_pair.find t.catch_up_managers feature_path for_ with
       | Some catch_up_manager ->
         (match Catch_up_manager.get_next_session catch_up_manager with
          | `Up_to_date -> `Up_to_date
          | `Catch_up_session session ->
            let is_archived =
              let feature_id = Catch_up_session.feature_id session in
              if Hashtbl.mem t.features_by_id feature_id
              then Is_archived.No
              else (
                match Archived_features.find_by_id t.archived_features feature_id with
                | Ok archived_feature -> Archived_feature.to_is_archived archived_feature
                | Error _ -> Yes { reason_for_archiving = "" })
            in
            let lines_required_to_separate_ddiff_hunks =
              lines_required_to_separate_ddiff_hunks t feature_path
                (* Don't fail to allow to catch-up on a feature whose root has been
                   archived. *)
                ~fail_if_root_does_not_exist:false
            in
            `Catch_up_session
              (Catch_up_manager.to_protocol catch_up_manager session ~is_archived
                 ~lines_required_to_separate_ddiff_hunks))
       | None ->
         if is_ok (Feature_forest.find t.features feature_path)
         || Archived_features.mem_feature_path t.archived_features feature_path
         then `Up_to_date
         else raise_s [%sexp "unknown feature", (feature_path : Feature_path.t)])
;;

let () =
  let module Dump = Iron_protocol.Dump in
  implement_rpc ~log:false Post_check_in_features.none
    (module Dump)
    (fun t query ->
       let review_managers feature of_ =
         list_of_iter (fun ~f ->
           iter_review_managers_of_feature t feature ~of_
             ~f:(fun _user review_manager -> f review_manager))
       in
       match Query.action query with
       | Archived_features_cache what_to_dump ->
         Archived_features.Cache.dump t.archived_features what_to_dump
       | Build_info -> Version_util.build_info_as_sexp
       | Version -> Version_util.version_list |> [%sexp_of: string list]
       | Worker_cache what_to_dump ->
         if Worker_cache.What_to_dump.require_admin_privileges what_to_dump
         then only_user_with_admin_privileges_is_authorized_exn t query;
         Worker_cache.dump t.worker_cache what_to_dump
       | State ->
         if not am_functional_testing
         then failwith "dumping state can only be done in test";
         t |> [%sexp_of: t]
       | Dynamic_upgrade_state ->
         Dynamic_upgrade.State.dump t.dynamic_upgrade_state
       | Push_events what_to_dump ->
         if Iron_protocol.Push_events.What_to_dump.require_admin_privileges what_to_dump
         then only_user_with_admin_privileges_is_authorized_exn t query;
         Push_events.dump t.push_events what_to_dump
       | Unclean_workspaces what_to_dump ->
         Unclean_workspaces_manager.dump t.unclean_workspaces what_to_dump
       | User_info which_user_info -> User_info.dump t.user_info which_user_info
       | Feature feature_path ->
         find_feature_exn t feature_path |> [%sexp_of: Feature.t]
       | Hash_consing_cache what_to_dump ->
         if Hash_consing.What_to_dump.require_admin_privileges what_to_dump
         then only_user_with_admin_privileges_is_authorized_exn t query;
         Hash_consing.dump (Hash_consing.the_one_and_only ()) what_to_dump
       | Event_subscriptions ->
         [%sexp
           { metric_updates  = (Metrics.dump_subscriptions t.metrics : Sexp.t)
           ; feature_updates =
               (Feature_updates_manager.dump_subscriptions t.feature_updates_manager
                : Sexp.t)
           }
         , (Event_subscriptions.dump t.event_subscriptions : Sexp.t)
         ]
       | Timed_event_table -> Timed_event.Table.dump t.timed_event_table
       | Review_analysis feature_path ->
         Feature.review_analysis (find_feature_exn t feature_path)
         |> [%sexp_of: Review_analysis.t option]
       | Review_manager (feature_path, user) ->
         let feature = find_feature_exn t feature_path in
         review_managers feature user |> [%sexp_of: Review_manager.t list]
       | Review_lines (feature_path, user) ->
         let feature = find_feature_exn t feature_path in
         let goal_subset = what_goal_subset_needs_to_be_reviewed feature in
         review_managers feature user
         |> List.map ~f:(fun review_manager ->
           Review_manager.dump_review_lines review_manager goal_subset)
         |> [%sexp_of: Sexp.t list]
       | Bookmarks_without_feature (r, u) ->
         let b = t.bookmarks_without_feature in
         match r, u with
         | None, `All_users -> b |> [%sexp_of: Bookmarks_without_feature.t]
         | Some remote_repo_path, `All_users ->
           Hashtbl2_pair.find1 b remote_repo_path
           |> [%sexp_of: (User_name.t, Bookmark_without_feature.t list) Hashtbl.t option]
         | None, `User user_name ->
           Hashtbl2_pair.find2 b user_name
           |> [%sexp_of: (Remote_repo_path.t, Bookmark_without_feature.t list
                         ) Hashtbl.t option]
         | Some remote_repo_path, `User user_name ->
           Hashtbl2_pair.find b remote_repo_path user_name
           |> [%sexp_of: Bookmark_without_feature.t list option]
    )
;;

let () =
  let module Is_fully_reviewed_edge = Iron_protocol.Is_fully_reviewed_edge in
  implement_rpc ~log:false Post_check_in_features.none
    (module Is_fully_reviewed_edge)
    (fun t query ->
       let { Is_fully_reviewed_edge.Action. from; to_ } = Query.action query in
       if not (Hashtbl2_pair.mem t.fully_reviewed_edges from to_)
       then
         raise_s
           [%sexp
             "not a fully-reviewed edge",
             { from : Rev.t
             ; to_  : Rev.t
             }
           ])
;;

let () =
  let module Enable_review = Iron_protocol.Enable_review in
  implement_rpc ~log:false
    (Post_check_in_features.one Enable_review.Action.feature_path)
    (module Enable_review)
    (fun t query ->
       let { Enable_review.Action.
             feature_path
           ; add_whole_feature_reviewers
           } = Query.action query
       in
       Option.iter add_whole_feature_reviewers ~f:(fun set ->
         User_info.ensure_users_exist t.user_info (Set.to_list set));
       let feature = find_feature_exn t feature_path in
       (match add_whole_feature_reviewers with
        | None -> ()
        | Some users ->
          change_feature_exn t feature query [ `Add_whole_feature_reviewers users ]);
       change_feature_exn t feature query
         [ `Set_crs_are_enabled   true
         ; `Set_review_is_enabled true
         ])
;;

let () =
  let module Fact_action = Iron_protocol.Fact_action in
  implement_rpc ~log:true Post_check_in_features.none
    (module Fact_action)
    (fun t query ->
       let action = Query.action query in
       let result = Fact.Db.handle_action t.fact_db action in
       if is_ok result
       then Serializer.append_to (serializer_exn t) action ~file:fact_actions_file
              (module Fact.Action.Persist);
       ok_exn result)
;;

let () =
  let module Fact_evidence = Iron_protocol.Fact_evidence in
  implement_rpc ~log:false Post_check_in_features.none
    (module Fact_evidence)
    (fun t query ->
       let (spec_id, scope) = Query.action query in
       ok_exn (Fact.Db.evidence t.fact_db spec_id scope))
;;

let () =
  let module Fact_list = Iron_protocol.Fact_list in
  implement_rpc ~log:false Post_check_in_features.none
    (module Fact_list)
    (fun t query ->
       let spec_id = Query.action query in
       ok_exn (Fact.Db.list_evidences t.fact_db spec_id))
;;

let () =
  let module Fact_spec = Iron_protocol.Fact_spec in
  implement_rpc ~log:false Post_check_in_features.none
    (module Fact_spec)
    (fun t query ->
       let spec_id = Query.action query in
       ok_exn (Fact.Db.spec t.fact_db spec_id))
;;

let () =
  let module Feature_description = Iron_protocol.Feature_description in
  implement_deferred_rpc ~log:false Post_check_in_features.none
    (module Feature_description)
    (fun t query ->
       let open Async in
       let%map feature =
         get_maybe_archived_feature_exn t (Query.action query)
       in
       { Feature_description.Reaction. description = feature.description })
;;

let () =
  let module List_fact_specs = Iron_protocol.List_fact_specs in
  implement_rpc ~log:false Post_check_in_features.none
    (module List_fact_specs)
    (fun t query ->
       let () = Query.action query in
       Fact.Db.list_specs t.fact_db)
;;

let () =
  let module Force_retry = Iron_protocol.Force_retry in
  implement_rpc ~log:true (Post_check_in_features.one Fn.id)
    (module Force_retry)
    (fun t query ->
       let feature_path = Query.action query in
       let feature = find_feature_exn t feature_path in
       if Feature.has_bookmark feature
       then Feature.force_hydra_retry feature
       else
         raise_s [%sexp "feature's bookmark is missing"
                      , (feature_path : Feature_path.t)])
;;

let () =
  let module Force_set_cached_feature_attributes =
    Iron_protocol.Force_set_cached_feature_attributes
  in
  implement_rpc ~log:true
    (Post_check_in_features.create
       (fun _ (action : Force_set_cached_feature_attributes.Action.t) ->
          let which_features =
            if action.skip_post_RPC_check
            then Which_features.Features []
            else Which_features.these_features [ action.feature_path ]
          in
          `run_after_reaction (fun () -> which_features)))
    (module Force_set_cached_feature_attributes)
    (fun t query ->
       let { Force_set_cached_feature_attributes.Action.
             feature_path
           ; skip_post_RPC_check = _
           ; next_steps
           } = Query.action query in
       let feature = find_feature_exn t feature_path in
       Cached.force_set_for_test_exn (Feature.next_steps_cached feature) (Ok next_steps))
;;

let () =
  let module Gc_compact = Iron_protocol.Gc_compact in
  implement_rpc ~log:true Post_check_in_features.none
    (module Gc_compact)
    (fun _t query ->
       let () = Query.action query in
       only_server_user_is_authorized_exn query;
       Gc.compact ())
;;

let () =
  let module Stat = Iron_protocol.Stat in
  implement_rpc ~log:true Post_check_in_features.none
    (module Stat)
    (fun _t query ->
       let { Stat.Action. kind } = Query.action query in
       match kind with
       | Gc_stat ->
         only_server_user_is_authorized_exn query;
         Gc.stat () |> [%sexp_of: Gc.Stat.t]
       | Gc_quick_stat ->
         Gc.quick_stat () |> [%sexp_of: Gc.Stat.t]
       | Process_times ->
         Unix.times () |> [%sexp_of: Core.Unix.process_times])
;;

let () =
  let module Supported_rpcs = Iron_protocol.Supported_rpcs in
  let reaction =
    lazy (
      List.map !rpc_implementations ~f:(fun impl ->
        let { Rpc.Description.name; version; } = Rpc.Implementation.description impl in
        Iron_protocol.Iron_versioned_rpc.find_rpc_exn ~name ~version))
  in
  implement_rpc ~log:true Post_check_in_features.none
    (module Supported_rpcs)
    (fun _t query ->
       let () = Query.action query in
       force reaction)
;;

let () =
  let module Pause = Iron_protocol.Persistent_state_serializer.Pause in
  implement_deferred_rpc ~log:true Post_check_in_features.none
    (module Pause)
    (fun t query ->
       only_server_user_is_authorized_exn query;
       let { Pause.Action. with_timeout } = Query.action query in
       let query = Query.with_action query () in
       let with_timeout =
         let max_timeout_span = Iron_config.serializer_pause_timeout t.server_config in
         if Time.Span.(>) with_timeout max_timeout_span
         then
           raise_s [%sexp "requested timeout is too big"
                        , { requested      = (with_timeout     : Time.Span.t)
                          ; max_configured = (max_timeout_span : Time.Span.t)
                          }]
         else with_timeout
       in
       Serializer.pause_exn (serializer_exn t) ~query ~with_timeout)
;;

let () =
  let module P =
    Iron_protocol.Persistent_state_serializer.Prior_changes_synced_to_file_system
  in
  implement_deferred_rpc ~log:false Post_check_in_features.none
    (module P)
    (fun t query ->
       let () = Query.action query in
       prior_changes_synced_to_file_system t)
;;

let () =
  let module Resume = Iron_protocol.Persistent_state_serializer.Resume in
  implement_rpc ~log:true Post_check_in_features.none
    (module Resume)
    (fun t query ->
       only_server_user_is_authorized_exn query;
       let () = Query.action query in
       Serializer.resume_exn (serializer_exn t))
;;

let () =
  let module Status = Iron_protocol.Persistent_state_serializer.Status in
  implement_rpc ~log:false Post_check_in_features.none
    (module Status)
    (fun t query ->
       let () = Query.action query in
       Serializer.pause_status (serializer_exn t))
;;

let () =
  let module Get_crs = Iron_protocol.Get_crs in
  implement_rpc ~log:false Post_check_in_features.none
    (module Get_crs)
    (fun t query ->
       let { Get_crs.Action. feature_path; for_or_all } = Query.action query in
       let feature = find_feature_exn t feature_path in
       match for_or_all with
       | `All_users -> feature_crs t feature
       | `User user_name ->
         match find_review_manager t feature user_name with
         | Error _ -> Ok []
         | Ok review_manager -> Review_manager.crs review_manager)
;;

let () =
  let module Get_cr_soons = Iron_protocol.Get_cr_soons in
  implement_rpc ~log:false Post_check_in_features.none
    (module Get_cr_soons)
    (fun t query ->
       let { Get_cr_soons.Action. root_feature; for_or_all; include_active } =
         Query.action query
       in
       Cr_soons.get_for_feature_tree t.cr_soons ~root_feature ~for_or_all ~include_active)
;;

let () =
  let module Get_cr_summary = Iron_protocol.Get_cr_summary in
  implement_rpc ~log:false Post_check_in_features.none
    (module Get_cr_summary)
    (fun t query ->
       let { Get_cr_summary.Action. feature_path } =
         Query.action query
       in
       feature_cr_summary t (find_feature_exn t feature_path))
;;

let () =
  let module Get_feature = Iron_protocol.Get_feature in
  implement_rpc ~log:false Post_check_in_features.none
    (module Get_feature)
    (fun t query ->
       let { Get_feature.Action. feature_path; rev_zero } = Query.action query in
       let protocol = feature_to_protocol ?rev_zero t (find_feature_exn t feature_path) in
       { protocol with
         diff_from_base_to_tip
         = Known (Or_error.error_string "diff not included in get-feature")
       ; latest_release = None
       })
;;

let () =
  let module Get_feature_by_id = Iron_protocol.Get_feature.By_id in
  implement_deferred_rpc ~log:false Post_check_in_features.none
    (module Get_feature_by_id)
    (fun t query ->
       let { Get_feature_by_id.Action. feature_id; even_if_archived }
         = Query.action query in
       get_maybe_archived_feature_exn t
         { feature_spec = `Feature_id feature_id
         ; namespace    = if even_if_archived then `All else `Existing
         })
;;

let () =
  let module Get_feature_revs = Iron_protocol.Get_feature_revs in
  implement_rpc ~log:false Post_check_in_features.none
    (module Get_feature_revs)
    (fun t query ->
       let { Get_feature_revs.Action. feature_path; rev_zero } = Query.action query in
       let feature = find_feature_exn t feature_path in
       let remote_repo_path = remote_repo_path t feature ?rev_zero in
       { base             = Feature.base feature
       ; tip              = Feature.tip feature
       ; remote_repo_path
       })
;;

let () =
  let module Get_feature_maybe_archived = Iron_protocol.Get_feature.Maybe_archived in
  implement_deferred_rpc ~log:false Post_check_in_features.none
    (module Get_feature_maybe_archived)
    (fun t query ->
       let open Async in
       let { Get_feature_maybe_archived.Action.
             what_feature
           ; what_diff
           } = Query.action query in
       let%map (_reviewer, feature_protocol) =
         get_maybe_archived_feature_and_reviewer_exn t ~what_feature ~what_diff
       in
       feature_protocol)
;;

let add_span_since_push_value_to_metric_if_available t ~feature ~tip ~metric_name =
  match Push_events.find t.push_events tip ~feature_id:(Feature.feature_id feature) with
  | None -> ()
  | Some push_event ->
    let feature_path = Feature.feature_path feature in
    if Feature_path.equal feature_path (Push_event.feature_path push_event)
    then (
      match Push_event.mark_as_used_by_metrics push_event metric_name with
      | `Already_marked -> ()
      | `Ok ->
        let span_since_push =
          Time.diff (Time.now ()) (Push_event.at push_event)
          |> Time.Span.to_sec
        in
        Metrics.add_values t.metrics
          { feature_path
          ; metric_name
          ; values       = [ span_since_push ]
          })
;;

let () =
  let module Hydra_worker = Iron_protocol.Hydra_worker in
  (* We log Hydra_worker even though it's not state changing, because it has information
     that may help us reconstruct what's going on in Update_bookmark, for which we elide
     information in the log if it's too large.  And, this is a small fraction of the size
     of Update_bookmark log entries. *)
  implement_rpc ~log:true (Post_check_in_features.one Hydra_worker.Action.feature_path)
    ~override_sexp_of_reaction:[%sexp_of: Hydra_worker.Reaction.Concise.t]
    (module Hydra_worker)
    (fun t query ->
       let { Hydra_worker.Action. feature_path; rev_zero; tip } = Query.action query in
       let feature = find_feature_exn t feature_path in
       Feature.set_has_bookmark feature query ~compilation_status:`Keep_any_known_value;
       (match Feature.next_base_update feature with
        | No_update_expected -> ()
        | Update_expected update_expected ->
          raise_s
            [%sexp
              "rejecting hydra worker query when a base update is expected",
              { update_expected : Next_base_update.Update_expected.t
              }
            ]);
       let (_ : Remote_repo_path.t) = remote_repo_path t feature ~rev_zero in
       let need_diff4s_starting_from = need_diff4s_starting_from t feature in
       let aliases = User_info.alternate_names t.user_info ~which:`Aliases in
       let lines_required_to_separate_ddiff_hunks =
         lines_required_to_separate_ddiff_hunks t feature_path
           ~fail_if_root_does_not_exist:true
       in
       let worker_cache =
         let feature_revs =
           let feature_revs =
             worker_cache_feature_revs t feature ~need_diff4s_starting_from
           in
           match tip with
           | Some tip -> { feature_revs with tip }
           | None -> feature_revs
         in
         Worker_cache.send_to_worker t.worker_cache feature_revs
       in
       { base       = Feature.base feature
       ; feature_id = Feature.feature_id feature
       ; need_diff4s_starting_from
       ; aliases
       ; lines_required_to_separate_ddiff_hunks
       ; worker_cache
       })
;;

let () =
  let module Get_alternate_names = Iron_protocol.Get_alternate_names in
  implement_rpc ~log:false Post_check_in_features.none
    (module Get_alternate_names)
    (fun t query ->
       User_info.alternate_names t.user_info ~which:(Query.action query))
;;

let () =
  let module Get_locked = Iron_protocol.Get_locked in
  implement_rpc ~log:false Post_check_in_features.none
    (module Get_locked)
    (fun t query ->
       let { Get_locked.Action. feature_path } = Query.action query in
       let feature = find_feature_exn t feature_path in
       Feature_locks.what_is_locked (Feature.locks feature))
;;

let () =
  implement_rpc ~log:true Post_check_in_features.none
    (module Iron_protocol.Metrics.Clear)
    (fun t query ->
       only_user_with_admin_privileges_is_authorized_exn t query;
       Metrics.clear t.metrics (Query.action query))
;;

let () =
  implement_rpc ~log:false Post_check_in_features.none
    (module Iron_protocol.Metrics.Get)
    (fun t query ->
       Metrics.get t.metrics (Query.action query))
;;

let () =
  implement_rpc ~log:false Post_check_in_features.none
    (module Iron_protocol.Push_events.Add)
    (fun t query ->
       let { Iron_protocol.Push_events.Add.Action. feature_id; _ } = Query.action query in
       let feature = find_feature_by_id_exn t feature_id in
       Push_events.add t.push_events query ~feature_path:(Feature.feature_path feature))
;;

let () =
  implement_rpc ~log:false Post_check_in_features.none
    (module Iron_protocol.Push_events.Change)
    (fun t query ->
       only_user_with_admin_privileges_is_authorized_exn t query;
       Push_events.change t.push_events (Query.action query))
;;

let () =
  implement_rpc ~log:false Post_check_in_features.none
    (module Iron_protocol.Metrics.Add_values)
    (fun t query ->
       let by = Query.by query in
       if User_info.Feeding_metrics.mem t.user_info by
       then Metrics.add_values t.metrics (Query.action query)
       else (
         let authorized_users = User_info.Feeding_metrics.get_set t.user_info in
         raise_s
           [%sexp
             "user is not authorized to feed metric values"
           , { user             = (by               : User_name.t)
             ; authorized_users = (authorized_users : User_name.Set.t)
             }
           ]))
;;

let () =
  let module Invalidate_cached_feature_attributes =
    Iron_protocol.Invalidate_cached_feature_attributes
  in
  implement_rpc ~log:true
    (Post_check_in_features.these_features Fn.id)
    (module Invalidate_cached_feature_attributes)
    (fun t query -> invalidate_features t (Query.action query))
;;

let () =
  let module Is_releasable = Iron_protocol.Is_releasable in
  implement_rpc ~log:false Post_check_in_features.none
    (module Is_releasable)
    (fun t query ->
       let { Is_releasable.Action. feature_path } = Query.action query in
       let feature = find_feature_exn t feature_path in
       let how_to_release =
         match parent_release_process t feature with
         | Direct     -> `Directly
         | Continuous -> `Push_to_hydra
       in
       ok_exn (check_releasable t feature ~how_to_release ~trust_cached_attributes:true))
;;

let () =
  let module List_features = Iron_protocol.List_features in
  implement_rpc ~log:false Post_check_in_features.none
    (module List_features)
    (fun t query ->
       let { List_features.Action. descendants_of; depth; use_archived } =
         Query.action query
       in
       if use_archived
       then
         ok_exn
           (Archived_features.list_features t.archived_features ~descendants_of ~depth)
       else
         ok_exn (Feature_forest.list t.features ~descendants_of ~depth)
         |> List.map ~f:(fun (feature_path, feature) ->
           { List_features.Reaction.
             feature_path
           ; feature_id        = Feature.feature_id feature
           ; owners            = Feature.owners feature
           ; review_is_enabled = Feature.review_is_enabled feature
           ; num_lines         =
               Feature.num_lines feature
               |> Or_pending.map ~f:(Result.map_error ~f:(fun _ ->
                 Error.of_string "problems in the feature"))
           ; next_steps        = Feature.next_steps feature
           ; status            = Existing
           }))
;;

let () =
  let module List_feature_names = Iron_protocol.List_feature_names in
  implement_rpc ~log:false Post_check_in_features.none
    (module List_feature_names)
    (fun t query ->
       let { List_feature_names.Action. descendants_of; depth; use_archived } =
         Query.action query
       in
       if use_archived
       then ok_exn (Archived_features.list_feature_names t.archived_features
                      ~descendants_of ~depth)
       else
         ok_exn (Feature_forest.list t.features ~descendants_of ~depth)
         |> List.map ~f:(fun (feature_path, _) -> feature_path))
;;

let () =
  let module List_feature_revisions = Iron_protocol.List_feature_revisions in
  implement_rpc ~log:false Post_check_in_features.none
    (module List_feature_revisions)
    (fun t query ->
       let { List_feature_revisions.Action. rev_zero; features; subtrees } =
         Query.action query
       in
       let reaction feature =
         { List_feature_revisions.Reaction.
           feature_path = Feature.feature_path feature
         ; base         = Feature.base feature
         ; tip          = Feature.tip  feature
         }
       in
       let subtree_features =
         List.map subtrees ~f:(fun feature_path ->
           ok_exn (Feature_forest.list t.features
                     ~descendants_of:(Feature feature_path) ~depth:Int.max_value)
           |> List.map ~f:(fun (_, feature) -> reaction feature))
       in
       let features =
         List.map features ~f:(fun feature_path ->
           reaction (find_feature_exn t feature_path))
       in
       let features =
         List.dedup_and_sort (List.concat (features :: subtree_features))
           ~compare:(fun (f1 : List_feature_revisions.Reaction.one) f2 ->
             Feature_path.compare f1.feature_path f2.feature_path)
       in
       match features with
       | [] -> failwith "must specify at least one feature"
       | { feature_path; _ } :: _ ->
         let root = Feature_path.root feature_path in
         List.iter features ~f:(fun { feature_path = feature_path2; _ } ->
           if not (Feature_name.equal root (Feature_path.root feature_path2)) then
             raise_s [%sexp "features must be in the same tree but aren't"
                          , ([ feature_path; feature_path2 ] : Feature_path.t list)]);
         let remote_repo_path =
           remote_repo_path t (find_feature_exn t feature_path) ~rev_zero
         in
         { List_feature_revisions.Reaction.
           remote_repo_path
         ; features
         })
;;

let () =
  let module List_root_features = Iron_protocol.List_root_features in
  implement_rpc ~log:false Post_check_in_features.none
    (module List_root_features) (fun t query ->
      let () = Query.action query in
      list_of_iter (fun ~f ->
        Feature_forest.iteri_roots t.features ~f:(fun root_feature feature ->
          f { List_root_features.Reaction.
              root_feature
            ; remote_repo_path = remote_repo_path t feature
            ; tip              = Feature.tip feature
            })))
;;

let () =
  let module Lock_feature = Iron_protocol.Lock_feature in
  implement_rpc ~log:true (Post_check_in_features.one Lock_feature.Action.feature_path)
    (module Lock_feature)
    (fun t query ->
       let { Lock_feature.Action.
             feature_path
           ; for_
           ; lock_names
           ; reason
           ; is_permanent
           } =
         Query.action query
       in
       User_info.ensure_user_exists t.user_info for_;
       let feature = find_feature_exn t feature_path in
       let for_ =
         if am_functional_testing
         then for_
         else (
           let requested_by = Query.by query in
           if User_name.equal requested_by for_
           then requested_by
           else
             raise_s
               [%sexp
                 "unauthorized lock with [-for]",
                 { requested_by  = (requested_by : User_name.t)
                 ; requested_for = (for_         : User_name.t)
                 }
               ])
       in
       List.map lock_names ~f:(fun lock_name ->
         (lock_name, Feature.lock feature ~query ~for_ ~lock_name ~reason ~is_permanent)))
;;

let () =
  let module Find_features_by_partial_name =
    Iron_protocol.Find_features_by_partial_name
  in
  implement_rpc ~log:false Post_check_in_features.none
    (module Find_features_by_partial_name)
    (fun t query ->
       let { Find_features_by_partial_name.Action. partial_name_prefix; namespace } =
         Query.action query
       in
       let match_ = Feature_path.match_ ~prefix:partial_name_prefix `Of_partial_name in
       let feature_path =
         Option.try_with (fun () ->
           String.try_chop_suffix partial_name_prefix ~suffix:"/"
           |> Feature_path.of_string)
       in
       let find (mem, iteri) =
         match feature_path with
         | Some feature_path when mem feature_path -> [feature_path]
         | Some _ | None ->
           let all_matches = Feature_path.Hash_set.create () in
           iteri ~f:(fun feature_path _ ->
             if is_some (force match_ feature_path)
             then Hash_set.add all_matches feature_path);
           Hash_set.to_list all_matches
       in
       let ops_on_archived =
         Archived_features.mem_feature_path t.archived_features,
         Archived_features.iteri t.archived_features
       in
       let ops_on_existing =
         Feature_forest.mem t.features,
         Feature_forest.iteri t.features
       in
       let ops_on_catch_up =
         Hashtbl2_pair.mem1 t.catch_up_managers,
         Hashtbl2_pair.iter1 t.catch_up_managers
       in
       let combine_ops (mem1, iteri1) (mem2, iteri2) =
         (fun feature_path -> mem1 feature_path || mem2 feature_path),
         (fun ~f -> iteri1 ~f:(fun p _ -> f p ()); iteri2 ~f:(fun p _ -> f p ()))
       in
       let sequence_ops (mem1, iteri1) (mem2, iteri2) =
         (fun feature_path -> mem1 feature_path || mem2 feature_path),
         (fun ~f ->
            let hit = ref false in
            iteri1 ~f:(fun p _ -> hit := true; f p ());
            if not !hit
            then iteri2 ~f:(fun p _ -> f p ()))
       in
       match namespace with
       | `Archived -> find ops_on_archived
       | `Existing -> find ops_on_existing
       | `All -> find (combine_ops ops_on_archived ops_on_existing)
       | `Existing_or_most_recently_archived ->
         find (sequence_ops ops_on_existing ops_on_archived)
       | `Existing_or_with_catch_up ->
         find (sequence_ops ops_on_existing ops_on_catch_up))
;;

let () =
  let module Mark_fully_reviewed = Iron_protocol.Mark_fully_reviewed in
  implement_rpc ~log:true
    (Post_check_in_features.one Mark_fully_reviewed.Action.feature_path)
    (module Mark_fully_reviewed)
    (fun t query ->
       let { Mark_fully_reviewed.Action.
             feature_path; whom_to_mark; reason; create_catch_up_for_me; base; tip }
         = Query.action query
       in
       let feature = find_feature_exn t feature_path in
       let check_rev name get_server_rev = function
         | None -> ()
         | Some provided_rev ->
           let server_rev = get_server_rev feature in
           if not (Rev.equal_node_hash server_rev provided_rev)
           then
             raise_s
               [%sexp
                 (sprintf "provided %s is different from what server knows"
                    name : string),
                 { provided_rev : Rev.t
                 ; server_rev   : Rev.t
                 }
               ]
       in
       check_rev "base" Feature.base base;
       check_rev "tip"  Feature.tip  tip;
       iter_review_managers_of_feature t feature ~of_:whom_to_mark
         ~f:(fun _user review_manager ->
           match
             review_authorization_may_skip_user_exn t feature review_manager query
               ~reason:(`This reason) ~create_catch_up_for_me
               ~is_reviewing_for:whom_to_mark
           with
           | `Error e -> Error.raise e
           | `Unauthorized_for_users_with_only_follow_lines _ -> ()
           | `Ok review_authorization ->
             Review_manager.mark_fully_reviewed review_manager query
               (`Review_authorization review_authorization)))
;;

let () =
  let module May_modify_others_catch_up = Iron_protocol.May_modify_others_catch_up in
  implement_rpc ~log:false Post_check_in_features.none
    (module May_modify_others_catch_up)
    (fun t query ->
       let { May_modify_others_catch_up.Action. for_ } =
         Query.action query
       in
       check_catch_up_for_exn t ~for_ ~by:(Query.by query))
;;

let () =
  let module May_modify_others_review = Iron_protocol.May_modify_others_review in
  implement_rpc ~log:false Post_check_in_features.none
    (module May_modify_others_review)
    (fun t query ->
       let { May_modify_others_review.Action. feature_path; whose_review; reason } =
         Query.action query
       in
       let feature = find_feature_exn t feature_path in
       let check_review_manager _ review_manager =
         match
           review_authorization_may_skip_user_exn t feature review_manager query
             ~reason ~create_catch_up_for_me:false ~is_reviewing_for:whose_review
         with
         | `Error e -> Error.raise e
         | `Ok (_ : Review_manager.Review_authorization.t)  -> ()
         | `Unauthorized_for_users_with_only_follow_lines _ -> ()
       in
       iter_review_managers_of_feature t feature ~of_:whose_review
         ~f:check_review_manager)
;;

let () =
  let module Ping = Iron_protocol.Ping in
  implement_rpc ~log:false Post_check_in_features.none
    (module Ping)
    (fun _t query ->
       Query.action query;
       ())
;;

let () =
  let module Prepare_for_crs = Iron_protocol.Prepare_for_crs in
  implement_rpc ~log:false Post_check_in_features.none
    (module Prepare_for_crs)
    (fun t query ->
       let { Prepare_for_crs.Action. feature_path } = Query.action query in
       let feature = find_feature_exn t feature_path in
       { owner_for_crs   = Feature.first_owner feature
       ; alternate_names = User_info.alternate_names t.user_info ~which:`All
       ; aliases         = User_info.alternate_names t.user_info ~which:`Aliases
       })
;;

let () =
  let module Prepare_to_rebase = Iron_protocol.Prepare_to_rebase in
  implement_rpc ~log:true Post_check_in_features.none
    (module Prepare_to_rebase)
    (fun t query ->
       let { Prepare_to_rebase.Action.
             feature_path = child_path
           ; rev_zero
           ; allow_non_cr_clean_new_base
           ; for_
           ; new_base
           } = Query.action query
       in
       let child = find_feature_exn t child_path in
       let may_rebase =
         Feature.is_owner child for_
         || is_ok (find_review_manager t child for_)
       in
       if not may_rebase then
         if User_name.equal for_ (Query.by query)
         then failwith "you are not allowed to rebase"
         else failwithf !"%{User_name} is not allowed to rebase" for_ ();
       ok_exn (Feature_locks.check_unlocked (Feature.locks child) Rebase);
       let remote_repo_path = remote_repo_path t child ~rev_zero in
       match Feature_path.parent child_path with
       | Error e -> Error.raise e
       | Ok parent_path ->
         let parent = find_feature_exn t parent_path in
         let parent_tip = Feature.tip parent in
         let child_tip = Feature.tip child in
         let pending since =
           failwithf "\
Iron server doesn't yet know whether it is valid to rebase (pending for %s)."
             (Time.Span.to_short_string (how_long ~since))
             ()
         in
         let new_base =
           match new_base with
           | Some new_base -> new_base
           | None ->
             match Feature.tip_facts parent with
             | Pending_since since -> pending since
             | Known parent_tip_facts ->
               let new_base = parent_tip in
               let new_base_facts = parent_tip_facts in
               if not (ok_exn (Rev_facts.Is_conflict_free.check
                                 new_base_facts.is_conflict_free new_base))
               then raise_s [%sexp "new base is not conflict free"
                                 , (parent_path : Feature_path.t)
                                 , (new_base    : Rev.t)];
               if not (ok_exn (Rev_facts.Obligations_are_valid.check
                                 new_base_facts.obligations_are_valid new_base))
               then raise_s [%sexp "new base obligations are invalid"
                                 , (parent_path : Feature_path.t)
                                 , (new_base    : Rev.t)];
               if not (ok_exn (Rev_facts.Is_cr_clean.check
                                 new_base_facts.is_cr_clean new_base))
               && not allow_non_cr_clean_new_base
               then failwith "\
new base is not cr clean -- use -allow-non-cr-clean-new-base to override";
               new_base
         in
         match Feature.tip_facts child with
         | Pending_since since -> pending since
         | Known child_tip_facts ->
           let old_base = Feature.base child in
           let old_tip = child_tip in
           let old_tip_facts = child_tip_facts in
           if Rev.equal_node_hash old_base new_base
           then raise_s [%sexp "feature is already rebased"
                             , (child_path : Feature_path.t)];
           if not (ok_exn (Rev_facts.Is_conflict_free.check
                             old_tip_facts.is_conflict_free child_tip))
           then raise_s [%sexp "feature is not conflict free"
                             , (child_path : Feature_path.t)
                             , (old_tip    : Rev.t)];
           { old_tip
           ; old_base
           ; new_base
           ; remote_repo_path
           ; feature_id       = Feature.feature_id child
           })
;;

let () =
  let module Prepare_to_restore_bookmark = Iron_protocol.Prepare_to_restore_bookmark in
  implement_rpc ~log:false Post_check_in_features.none
    (module Prepare_to_restore_bookmark)
    (fun t query ->
       let { Prepare_to_restore_bookmark.Action. feature_path; rev_zero } =
         Query.action query
       in
       let feature = find_feature_exn t feature_path in
       (if Feature.has_bookmark feature
        then
          (* There is a little race on startup because before receiving the first
             synchronize state query we consider [has_bookmark = true].  But this is for
             the best, we wait at least one synchronize state query before allowing to
             restore bookmarks. *)
          raise_s [%sexp "feature has a bookmark"
                       , (Feature.feature_path feature : Feature_path.t)]);
       { tip              = Feature.tip feature
       ; remote_repo_path = remote_repo_path t feature ~rev_zero
       })
;;

let () =
  let module Prepare_to_second = Iron_protocol.Prepare_to_second in
  implement_rpc ~log:true Post_check_in_features.none
    (module Prepare_to_second)
    (fun t query ->
       let { Prepare_to_second.Action. feature_path; even_though_empty; even_though_owner } =
         Query.action query
       in
       let feature = find_feature_exn t feature_path in
       let by = Query.by query in
       ok_exn (seconding_allowed feature ~by ~even_though_empty ~even_though_owner
                 ~even_if_locked:false);
       let whole_feature_reviewers = Feature.whole_feature_reviewers feature in
       let whole_feature_review_remaining =
         compute_line_count_by_user_exn t feature
         |> List.filter ~f:(fun (user, line_count) ->
           Set.mem whole_feature_reviewers user
           && Review_or_commit.count (Line_count.to_review_column_shown line_count) > 0)
       in
       let cr_summary = ok_exn (feature_cr_summary t feature) in
       { whole_feature_review_remaining; cr_summary })
;;

let () =
  let module Release = Iron_protocol.Release in
  implement_rpc ~log:true
    (Post_check_in_features.create
       (fun t (action : Release.Action.t) ->
          let feature_path = action.feature_path in
          `run_after_reaction (fun () ->
            let released_feature_if_present =
              if is_ok (find_feature t feature_path)
              then [ feature_path ]
              else []
            in
            Which_features.these_features
              (match Feature_path.parent feature_path with
               | Error _ -> released_feature_if_present
               | Ok parent -> parent :: released_feature_if_present))))
    (module Release) (fun t query ->
      let { Release.Action.
            feature_path
          ; rev_zero
          ; tagged_tip
          ; for_
          } = Query.action query
      in
      let by = Query.by query in
      let feature = find_feature_exn t feature_path in
      let feature_tip = Feature.tip feature in
      let parent =
        match Feature_path.parent feature_path with
        | Error _        -> None
        | Ok parent_path -> Some (find_feature_exn t parent_path)
      in
      let query_is_for_hydra = User_name.equal for_ t.server_config.hydra_user in
      let query_is_by_hydra  = User_name.equal by   t.server_config.hydra_user in
      (if not query_is_for_hydra
       then (
         let child_owners = Feature.owners feature in
         let who_can_release =
           match parent with
           | None -> child_owners
           | Some parent ->
             let parent_owners = Feature.owners parent in
             match Feature.who_can_release_into_me parent with
             | My_owners                  -> parent_owners
             | My_owners_and_child_owners -> parent_owners @ child_owners
         in
         if not (List.mem who_can_release for_ ~equal:User_name.equal)
         then
           raise_s
             [%sexp "only these users may release"
                  , (User_name.Set.of_list who_can_release : User_name.Set.t)
             ]));
      let parent_release_process = parent_release_process t feature in
      let send_release_email_to =
        Email_address.Set.union_list
          [ feature_email feature ~sent_upon:Release
          ; Option.value_map parent ~default:Email_address.Set.empty
              ~f:(feature_email ~sent_upon:Release_into)
          ; (match parent_release_process with
             | Continuous ->
               (* In this case, hydra is releasing directly after having validated the
                  feature -- we don't want to send it mail. *)
               Email_address.Set.empty
             | Direct ->
               [ by; for_ ]
               |> List.map ~f:Email_address.of_user_name
               |> Email_address.Set.of_list)
          ]
      in
      let how_to_release =
        match parent_release_process, query_is_for_hydra with
        | Direct    , _     -> `Directly
        | Continuous, false -> `Push_to_hydra
        | Continuous, true  ->
          if not query_is_by_hydra
          then only_user_with_admin_privileges_is_authorized_exn t query;
          `Directly
      in
      let reasons_for_not_archiving =
        List.concat
          [ if Feature.is_permanent feature then [ `Feature_is_permanent ] else []
          ; if Feature_forest.has_children_exn t.features feature_path
            then [ `Feature_has_children ]
            else []
          ]
      in
      let should_keep_feature = not (List.is_empty reasons_for_not_archiving) in
      if should_keep_feature
      then ok_exn (Feature_locks.check_unlocked (Feature.locks feature) Rebase);
      ok_exn (check_releasable t feature ~how_to_release ~trust_cached_attributes:false);
      let released_child = Feature.to_released_feature_exn feature ~query ~tagged_tip in
      let diff_from_base_to_tip = ok_known_exn (Feature.diff_from_base_to_tip feature) in
      (* After this point we are committed to the release *)
      let disposition =
        match how_to_release with
        | `Push_to_hydra -> `Not_released__push_to_hydra
        | `Directly ->
          add_fully_reviewed_edge t
            { Fully_reviewed_edge.
              rev_zero
            ; from_    = Feature.base feature
            ; to_      = feature_tip
            ; reason   = Review
            };
          (match parent with
           | None -> ()
           | Some parent ->
             (* Adding this edge is correct because [check_releasable] checks that the
                parent is reviewed. Note that there isn't necessarily a fully reviewed
                edge for these revisions, since we only create those on release, not every
                time a feature is fully reviewed. *)
             add_fully_reviewed_edge t
               { Fully_reviewed_edge.
                 rev_zero
               ; from_    = Feature.base parent
               ; to_      = feature_tip
               ; reason   = Release (feature_path, Feature.base feature)
               });
          (match parent with
           | None -> ()
           | Some parent -> Feature.include_released_feature parent released_child);
          Feature.set_latest_release feature
            { released_feature      = released_child
            ; diff_from_base_to_tip
            };
          Feature.clear_included_features feature;
          Feature.set_base feature query feature_tip;
          iter_review_managers_of_feature t feature ~f:(fun _ review_manager ->
            Review_manager.release review_manager query);
          if should_keep_feature
          then
            `Released_and_cleared
              (Release.Reasons_for_not_archiving.create reasons_for_not_archiving)
          else (
            archive_feature t query feature ~for_ ~must_be_owner:false
              ~reason_for_archiving:"released";
            `Released_and_archived)
      in
      { disposition
      ; send_release_email_to
      })
;;

(* [revision_is_fully_reviewed t rev] returns [true] iff there is a path in
   [t.fully_reviewed_edges] from a rev in [t.fully_reviewed_revisions] to [rev].  It works
   by searching backward from [rev]. *)
let revision_is_fully_reviewed t rev =
  let seen = Rev.Compare_by_hash.Hash_set.create () in
  with_return (fun r ->
    let rec loop rev : unit =
      if not (Hash_set.mem seen rev)
      then (
        if Hash_set.mem t.fully_reviewed_revisions rev
        then r.return true
        else (
          Hash_set.add seen rev;
          (* [t.fully_reviewed_edges] is a directed graph and we are searching backward,
             so we use [find2_iter1] to traverse all edges to [rev]. *)
          Hashtbl2_pair.find2_iter1 t.fully_reviewed_edges rev
            ~f:(fun from_ _ -> loop from_)))
    in
    loop rev;
    false)
;;

let () =
  let module Revision_is_fully_reviewed = Iron_protocol.Revision_is_fully_reviewed in
  implement_rpc ~log:false Post_check_in_features.none
    (module Revision_is_fully_reviewed)
    (fun t query ->
       let { Revision_is_fully_reviewed.Action. rev } = Query.action query in
       if not (revision_is_fully_reviewed t rev)
       then raise_s [%sexp "revision is not fully reviewed", (rev : Rev.t)])
;;

let () =
  let module Reviewed_diffs = Iron_protocol.Reviewed_diffs in
  implement_rpc ~log:true (Post_check_in_features.one Reviewed_diffs.Action.feature_path)
    (module Reviewed_diffs)
    (fun t query ->
       let { Reviewed_diffs.Action.
             feature_path
           ; for_; reason; review_session_id; diff4_in_session_ids
           ; create_catch_up_for_me
           ; even_if_some_files_are_already_reviewed
           } =
         Query.action query
       in
       let feature = find_feature_exn t feature_path in
       let review_manager = ok_exn (find_review_manager t feature for_) in
       let review_authorization =
         review_authorization_exn t feature review_manager query
           ~reason:(`This reason) ~create_catch_up_for_me
       in
       let goal_subset = what_goal_subset_needs_to_be_reviewed feature in
       Review_manager.reviewed review_manager query review_session_id
         diff4_in_session_ids goal_subset review_authorization
         ~even_if_some_files_are_already_reviewed
       |> ok_exn)
;;

let () =
  let module Catch_up_diffs = Iron_protocol.Catch_up_diffs in
  implement_rpc ~log:true
    (Post_check_in_features.one_if_present Catch_up_diffs.Action.feature_path)
    (module Catch_up_diffs)
    (fun t query ->
       let { Catch_up_diffs.Action.
             feature_path; for_; catch_up_session_id = id; diff4_in_session_ids } =
         Query.action query
       in
       check_catch_up_for_exn t ~for_ ~by:(Query.by query);
       match Hashtbl2_pair.find t.catch_up_managers feature_path for_ with
       | None -> raise_s [%sexp "no pending catch up for user", (for_ : User_name.t)]
       | Some catch_up_manager ->
         match Catch_up_manager.find catch_up_manager id with
         | None -> raise_s [%sexp "invalid catch-up session id", (id : Session_id.t)]
         | Some catch_up_session ->
           catch_up_in_session t query feature_path catch_up_manager catch_up_session
             diff4_in_session_ids for_)
;;

let () =
  let module Unsecond = Iron_protocol.Unsecond in
  implement_rpc ~log:true (Post_check_in_features.one Unsecond.Action.feature_path)
    (module Unsecond)
    (fun t query ->
       let { Unsecond.Action. feature_path; for_ } = Query.action query in
       let feature = find_feature_exn t feature_path in
       match Feature.seconder feature with
       | None           -> failwith "the feature is not seconded"
       | Some seconder ->
         let may_unsecond = User_name.equal for_ seconder in
         if not may_unsecond then
           if User_name.equal for_ (Query.by query)
           then failwith "you are not allowed to unsecond"
           else failwithf !"%{User_name} is not allowed to unsecond" for_ ();
         ok_exn (Feature.set_seconder feature query None);
         change_feature_exn t feature query [ `Set_reviewing `Whole_feature_reviewers ])
;;

let () =
  let module Second = Iron_protocol.Second in
  implement_rpc ~log:true  (Post_check_in_features.one Second.Action.feature_path)
    (module Second)
    (fun t query ->
       let { Second.Action. feature_path; even_though_empty; even_though_owner } =
         Query.action query
       in
       let feature = find_feature_exn t feature_path in
       let by = Query.by query in
       ok_exn (seconding_allowed feature ~by ~even_though_empty ~even_though_owner
                 ~even_if_locked:false);
       ok_exn (Feature.set_seconder feature query (Some by));
       change_feature_exn t feature query [ `Set_reviewing `All ])
;;

let () =
  let module Server_started_at = Iron_protocol.Server_started_at in
  implement_rpc ~log:false Post_check_in_features.none
    (module Server_started_at)
    (fun _t query ->
       let () = Query.action query in
       { server_started_at = program_started_at })
;;

let set_brains_to_goal_if_edge t query feature =
  if is_fully_reviewed_edge t ~base:(Feature.base feature) ~tip:(Feature.tip feature)
  then
    iter_review_managers_of_feature t feature ~f:(fun _ review_manager ->
      Review_manager.mark_fully_reviewed review_manager query
        `Internal_mark__no_catch_up_allow_for_all)
;;

let () =
  let module Set_brains_to_goal_if_edge = Iron_protocol.Set_brains_to_goal_if_edge in
  implement_rpc ~log:true
    (Post_check_in_features.one Set_brains_to_goal_if_edge.Action.feature_path)
    (module Set_brains_to_goal_if_edge)
    (fun t query ->
       let { Set_brains_to_goal_if_edge.Action. feature_path } =
         Query.action query
       in
       let feature = find_feature_exn t feature_path in
       set_brains_to_goal_if_edge t query feature)
;;

let () =
  let users_to_unlock feature lock_name =
    Next_step.Lock_name.to_lock_name lock_name
    |> Feature_locks.find (Feature.locks feature)
    |> List.map ~f:Feature_locks.Locked.by
    |> User_name.Set.of_list
  in
  let users_for_parent_next_step ~next_step_in_parent ~parent =
    match (next_step_in_parent : Next_step.t) with
    | Unlock Release_into -> users_to_unlock parent Release_into
    | Add_code
    | Add_whole_feature_reviewer
    | Archive
    | Ask_seconder
    | Compress
    | CRs
    | Enable_review
    | Fix_build
    | Fix_problems
    | Rebase
    | Release
    | Report_iron_bug
    | Restore_base
    | Restore_bookmark
    | Review
    | Wait_for_hydra
    | Widen_reviewing
    | In_parent _
    | Unlock Rebase
    | Unlock Release
    | Unlock Second
    | Wait_for_continuous_release -> User_name.Set.empty
  in
  let module Remind = Iron_protocol.Remind in
  implement_rpc ~log:true Post_check_in_features.none (module Remind) (fun t query ->
    let { Remind.Action. feature_path; users } = Query.action query in
    let feature = find_feature_exn t feature_path in
    let line_count_by_user =
      compute_line_count_by_user_exn t feature
      |> List.filter ~f:(fun (user, line_count) ->
        Feature.user_is_currently_reviewing feature user
        && Review_or_commit.count (Line_count.to_review_column_shown line_count) > 0)
    in
    let review_users =
      List.map line_count_by_user ~f:fst
      |> User_name.Set.of_list
    in
    let cr_summary = ok_exn (feature_cr_summary t feature) in
    let cr_users =
      List.map (Cr_comment.Summary.rows cr_summary) ~f:Cr_comment.Summary.Row.assignee
      |> User_name.Set.of_list
    in
    let users_with_review_session_in_progress =
      Feature.users_with_review_session_in_progress feature
    in
    let users_with_unclean_workspaces =
      users_with_unclean_workspaces t feature_path
    in
    let owners = User_name.Set.of_list (Feature.owners feature) in
    let next_step_users =
      List.map (Feature.next_steps feature) ~f:(function
        (* Since we will union at the end, we produce here every user that makes sense
           to produce, even if we know they will be produced elsewhere. *)
        | Add_code
        | Add_whole_feature_reviewer
        | Archive
        | Compress
        | Enable_review
        | Fix_build
        | Fix_problems
        | Rebase
        | Report_iron_bug
        | Restore_base
        | Restore_bookmark
        | Widen_reviewing -> owners
        | Release ->
          (match assigned_release_or_rebase t feature with
           | None -> owners
           | Some { assignee; assigned = _ } -> Set.add owners assignee)
        | Ask_seconder -> Feature.whole_feature_reviewers feature
        | CRs -> cr_users
        | In_parent next_step_in_parent ->
          (match Feature_path.parent (Feature.feature_path feature) with
           | Error error ->
             raise_s [%sexp "Iron bug.  Root feature has next step [In_parent _]."
                          , (Feature.next_steps feature : Next_step.t list)
                          , (Feature.feature_path feature : Feature_path.t)
                          , (error : Error.t)]
           | Ok parent_path ->
             users_for_parent_next_step
               ~next_step_in_parent
               ~parent:(find_feature_exn t parent_path))
        | Review -> review_users
        | Unlock lock_name -> users_to_unlock feature lock_name
        | Wait_for_hydra
        | Wait_for_continuous_release -> User_name.Set.empty)
      |> User_name.Set.union_list
    in
    let active_users =
      User_name.Set.union_list
        [ cr_users
        ; next_step_users
        ; owners
        ; review_users
        ; (match users_with_review_session_in_progress with
           | Error _ -> User_name.Set.empty
           | Ok set -> set)
        ; users_with_unclean_workspaces |> User_name.Set.of_map_keys
        ]
    in
    let requested_active_users =
      match users with
      | All_active -> active_users
      | Some_active only_email ->
        let active, inactive = Set.partition_tf only_email ~f:(Set.mem active_users) in
        if not (Set.is_empty inactive)
        then raise_s [%sexp "some requested users have nothing to do"
                          , (inactive : User_name.Set.t)];
        active
    in
    let users = Set.union requested_active_users owners in
    { Remind.Reaction.
      description        = Feature.description feature
    ; line_count_by_user
    ; users_with_review_session_in_progress
    ; users_with_unclean_workspaces
    ; cr_summary
    ; users
    ; next_bookmark_update = Feature.next_bookmark_update feature
    })
;;

let () =
  let module Synchronize_state = Iron_protocol.Synchronize_state in
  implement_rpc ~log:false
    (* What features change is complicated -- we handle invalidation on a per-feature
       basis in [Feature.synchronize_with_hydra]. *)
    Post_check_in_features.none
    (module Synchronize_state)
    (fun t query ->
       let { Synchronize_state.Action. remote_repo_path; bookmarks } =
         Query.action query
       in
       let root = ok_exn (find_root_by_remote_repo_path t remote_repo_path) in
       let state_by_bookmark =
         String.Table.of_alist_exn
           (List.map bookmarks ~f:(fun ({ bookmark; _ } as r) -> bookmark, r))
       in
       let bookmarks_to_rerun = Queue.create () in
       Feature_forest.iter_descendants t.features (Feature.feature_path root)
         ~f:(fun feature ->
           let feature_path = Feature.feature_path feature in
           let bookmark = Feature_path.to_string feature_path in
           match Hashtbl.find_and_remove state_by_bookmark bookmark with
           | None -> Feature.set_has_no_bookmark feature query;
           | Some { bookmark = _; rev_info = { rev_author_or_error = _; first_12_of_rev }
                  ; status; continuous_release_status; compilation_status } ->
             let next_bookmark_update_before = Feature.next_bookmark_update feature in
             Feature.set_continuous_release_status feature continuous_release_status;
             Feature.set_has_bookmark feature query
               ~compilation_status:(`Update_with compilation_status);
             (match
                Feature.synchronize_with_hydra feature
                  ~hydra_tip:first_12_of_rev
                  ~hydra_status:status
              with
              | `Retry        -> Queue.enqueue bookmarks_to_rerun bookmark;
              | `Do_not_retry -> ());
             if Next_bookmark_update.is_transition_to_update_expected
                  ~from:next_bookmark_update_before
                  ~to_:(Feature.next_bookmark_update feature)
             then (
               add_span_since_push_value_to_metric_if_available t ~feature
                 ~tip:first_12_of_rev
                 ~metric_name:Metric_name.hydra_synchronize_state_latency)
         );
       (let bookmarks_without_feature =
          Hashtbl.data state_by_bookmark
          |> List.filter_map ~f:
               (fun { bookmark; rev_info = { rev_author_or_error; first_12_of_rev }
                    ; status = _; continuous_release_status = _
                    ; compilation_status = _ } ->
                 match rev_author_or_error with
                 | Error _ -> None
                 | Ok rev_author ->
                   Some (rev_author,
                         { Bookmark_without_feature. bookmark; first_12_of_rev }))
          |> User_name.Map.of_alist_multi
        in
        Hashtbl2_pair.remove_all1 t.bookmarks_without_feature remote_repo_path;
        Map.iteri bookmarks_without_feature ~f:(fun ~key:user ~data:info ->
          Hashtbl2_pair.add_exn t.bookmarks_without_feature remote_repo_path user info));
       { Synchronize_state.Reaction. bookmarks_to_rerun = Queue.to_list bookmarks_to_rerun }
    )
;;

let () =
  let module Todo = Iron_protocol.Todo in
  implement_rpc ~log:false Post_check_in_features.none
    (module Todo)
    (fun t query ->
       let { Todo.Action. for_; include_active_cr_soons; include_all_owned_features }
         = Query.action query in
       User_info.ensure_user_exists t.user_info for_;
       let all_owned_feature_ids =
         Features_by_parties.find t.features_by_parties for_ Owners
       in
       let features_not_owned_yet_assigned_to_release =
         let table = Feature_id.Table.create () in
         List.iter all_owned_feature_ids ~f:(fun feature_id ->
           let feature = find_feature_by_id_exn t feature_id in
           match Feature.who_can_release_into_me feature with
           | My_owners_and_child_owners -> ()
           | My_owners ->
             Feature_forest.iter_children t.features (Feature.feature_path feature)
               ~f:(fun child ->
                 if List.mem (Feature.next_steps child) Release ~equal:Next_step.equal
                 then (
                   match
                     Assigned_release_or_rebase.create child ~parent:(Some feature)
                   with
                   | None -> ()
                   | Some { assignee; assigned } ->
                     if User_name.(=) for_ assignee
                     then Hashtbl.set table ~key:(Feature.feature_id child)
                            ~data:(child, assigned))));
         table
       in
       let assigned =
         let catch_up_lines_by_feature = build_catch_up_lines_by_feature t for_ in
         (* Make the initial set of [assigned] table rows from the user's review managers.
            As we do this, we evacuate [catch_up_lines_by_feature]. *)
         let r = ref [] in
         let crs_are_enabled_for_user_in feature for_users_reviewing_only =
           Feature.crs_are_enabled feature
           && (not (for_users_reviewing_only feature)
               || Feature.user_is_currently_reviewing feature for_)
         in
         iter_review_managers_of_user t for_ ~f:(fun feature_id review_manager ->
           Hashtbl.remove features_not_owned_yet_assigned_to_release feature_id;
           let feature = find_feature_by_id_exn t feature_id in
           let feature_path = Feature.feature_path feature in
           let make_num_crs for_users_reviewing_only crs =
             if crs_are_enabled_for_user_in feature for_users_reviewing_only
             then `Enabled (crs review_manager)
             else `Disabled
           in
           let num_crs =
             make_num_crs Feature.crs_shown_in_todo_only_for_users_reviewing
               Review_manager.num_crs
           in
           let num_xcrs =
             make_num_crs Feature.xcrs_shown_in_todo_only_for_users_reviewing
               Review_manager.num_xcrs
           in
           let line_count =
             let catch_up =
               Hashtbl.find_and_remove catch_up_lines_by_feature feature_path
               |> Option.value ~default:Line_count.Catch_up.zero
             in
             let in_feature =
               match Feature.line_count_by_user feature with
               | Error _ -> None
               | Ok map -> Map.find map for_
             in
             match in_feature with
             | None -> Line_count.catch_up_only catch_up
             | Some { review
                    ; completed
                    ; have_potentially_blocking_review_session_in_progress
                    } ->
               { Line_count.
                 review = To_goal_via_session.maybe_partially_known review
               ; catch_up
               ; completed
               ; have_potentially_blocking_review_session_in_progress
               }
           in
           let review_is_enabled = Feature.review_is_enabled feature in
           let user_is_reviewing = Feature.user_is_currently_reviewing feature for_ in
           let next_steps = Feature.next_steps feature in
           let assigned_next_steps : Next_step.Assigned.t =
             (* We need this special case for [Ask_seconder] because the heuristic to
                chose whom to assign this next step is not straight forward.  For example,
                the owner does not get a 'second' in their todo if there is another
                potential seconder who's not an owner -- Iron does not encourage users to
                [second -even-though-owner].  *)
             if seconding_is_recommended t ~for_ feature ~even_if_locked:false
             then [ Ask_seconder ]
             else (
               let need_rebase, next_steps_skipping_rebase =
                 match next_steps with
                 | Rebase :: ts -> true, ts
                 | ts -> false, ts
               in
               let maybe_prepend_rebase t =
                 if need_rebase
                 then [ Next_step.Rebase; t ]
                 else [ t ]
               in
               let only_first_owner =
                 if User_name.equal for_ (Feature.first_owner feature)
                 then Fn.id
                 else const []
               in
               match List.hd next_steps_skipping_rebase with
               | None -> []
               | Some next_step ->
                 match next_step with
                 | Add_whole_feature_reviewer
                 | Report_iron_bug
                 | Restore_base
                 | Restore_bookmark
                 | Widen_reviewing
                   -> only_first_owner (maybe_prepend_rebase next_step)

                 | Fix_build
                 | Fix_problems
                   ->
                   if review_is_enabled
                   then only_first_owner (maybe_prepend_rebase next_step)
                   else []

                 | Release ->
                   (match assigned_release_or_rebase t feature with
                    | None -> []
                    | Some { assignee; assigned } ->
                      if User_name.(<>) for_ assignee
                      then []
                      else assigned)

                 | Add_code
                 | Archive
                 | Ask_seconder
                 | Compress
                 | CRs
                 | Enable_review
                 | In_parent _
                 | Rebase
                 | Review
                 | Unlock _
                 | Wait_for_continuous_release
                 | Wait_for_hydra
                   -> [])
           in
           let need_to_show_crs = function
             | `Disabled -> false
             | `Enabled Error _ -> false
             | `Enabled (Ok n) -> n > 0
           in
           let need_to_show =
             need_to_show_crs num_crs
             || need_to_show_crs num_xcrs
             || (user_is_reviewing
                 && Line_count.is_shown line_count ~show_completed_review:false)
             || Line_count.Catch_up.total line_count.catch_up > 0
             || not (List.is_empty assigned_next_steps)
           in
           if need_to_show
           then (
             let assigned =
               { Todo.Assigned.
                 feature_path
               ; feature_path_exists = true
               ; review_is_enabled
               ; user_is_reviewing
               ; assigned_next_steps
               ; num_crs
               ; num_xcrs
               ; line_count
               ; next_steps
               }
             in
             r := assigned :: !r));
         (* Add rows for all surviving elements in the [catch_up_lines_by_feature]. *)
         Hashtbl.iteri catch_up_lines_by_feature ~f:(fun ~key:feature_path ~data ->
           let assigned =
             { Todo.Assigned.
               feature_path
             ; feature_path_exists = Result.is_ok (find_feature t feature_path)
             ; review_is_enabled   = false
             ; user_is_reviewing   = false
             ; assigned_next_steps = []
             (* Maybe there is an existing feature with that feature_path, however if the
                user had some assigments other than catch-up for it the entry would have
                already been removed from [catch_up_lines_by_feature] and the feature
                already processed in the iteration by review managers above.  Thus here we
                only populate the catch up lines.  That's all that matters to that user
                for that feature_path. *)
             ; num_crs             = `Disabled
             ; num_xcrs            = `Disabled
             ; line_count          = Line_count.catch_up_only data
             ; next_steps          = []
             }
           in
           r := assigned :: !r);
         (* Add surviving elements of [assigned_to_release_instead_of_child_owner]. *)
         Hashtbl.iter features_not_owned_yet_assigned_to_release
           ~f:(fun (feature, assigned_next_steps) ->
             let assigned =
               { Todo.Assigned.
                 feature_path        = Feature.feature_path feature
               ; feature_path_exists = true
               ; review_is_enabled   = Feature.review_is_enabled feature
               ; user_is_reviewing   = Feature.user_is_currently_reviewing feature for_
               ; assigned_next_steps
               (* If that user had CRs or lines to review, they would have a
                  review-manager and this feature would have been processed as part of the
                  previous review_manager iteration. *)
               ; num_crs             = `Disabled
               ; num_xcrs            = `Disabled
               ; line_count          = Line_count.zero
               ; next_steps          = Feature.next_steps feature
               }
             in
             r := assigned :: !r);
         !r
       in
       let unclean_workspaces =
         Unclean_workspaces_manager.find_user t.unclean_workspaces for_
       in
       let make_feature_info feature_id =
         let feature = find_feature_by_id_exn t feature_id in
         let feature_path = Feature.feature_path feature in
         let num_crs = ref (Ok 0) in
         let num_xcrs = ref (Ok 0) in
         let num_reviewers_with_review_remaining = ref (Ok 0) in
         let add r int_or_error =
           match !r with
           | Error _ -> ()
           | Ok n ->
             match int_or_error with
             | Error _ as e -> r := e
             | Ok m -> r := Ok (n + m)
         in
         let line_count_by_user = Feature.line_count_by_user feature in
         iter_review_managers_of_feature t feature
           ~f:(fun user review_manager ->
             add num_crs  (Review_manager.num_crs  review_manager);
             add num_xcrs (Review_manager.num_xcrs review_manager);
             (match !num_reviewers_with_review_remaining with
              | Error _ -> ()
              | Ok n ->
                Or_error.try_with (fun () ->
                  match Map.find (ok_exn line_count_by_user) user with
                  | None -> ()
                  | Some line_count ->
                    let review =
                      Line_count.Review.to_review_column_shown
                        (To_goal_via_session.fully_known_exn line_count.review)
                        ~have_potentially_blocking_review_session_in_progress:
                          line_count.have_potentially_blocking_review_session_in_progress
                    in
                    if Review_or_commit.count review > 0
                    then num_reviewers_with_review_remaining := Ok (n + 1))
                |> function
                | Ok () -> ()
                | Error err -> num_reviewers_with_review_remaining := Error err));
         let rev_facts rev facts =
           Or_pending.map facts ~f:(fun (facts : Rev_facts.t) ->
             let one check fact =
               match check fact rev with
               | Ok _ as o -> o
               | Error _ -> Error ()
             in
             { Todo.Rev_facts.
               is_conflict_free =
                 one Rev_facts.Is_conflict_free.check facts.is_conflict_free
             ; is_cr_clean = one Rev_facts.Is_cr_clean.check facts.is_cr_clean
             ; obligations_are_valid =
                 one Rev_facts.Obligations_are_valid.check facts.obligations_are_valid
             })
         in
         let base = rev_facts (Feature.base feature) (Feature.base_facts feature) in
         let tip  = rev_facts (Feature.tip  feature) (Feature.tip_facts  feature) in
         { Todo.Feature_info.
           feature_path
         ; num_crs                             = !num_crs
         ; num_xcrs                            = !num_xcrs
         ; num_reviewers_with_review_remaining =
             !num_reviewers_with_review_remaining
         ; base
         ; tip
         ; review_is_enabled                   = Feature.review_is_enabled feature
         ; next_steps                          = Feature.next_steps feature
         }
       in
       let can_be_hidden feature =
         let is_empty_umbrella_feature () =
           List.exists (Feature.next_steps feature)
             ~f:(function Add_code -> true | _ -> false)
           && (Feature.is_permanent feature
               || Feature_forest.has_children_exn t.features
                    (Feature.feature_path feature))
         in
         let is_permanently_locked lock_name =
           Feature_locks.is_permanently_locked (Feature.locks feature)
             (Next_step.Lock_name.to_lock_name lock_name)
         in
         let is_permanently_locked_and_stable () =
           match Feature.next_steps feature with
           | [ Unlock lock_name ] -> is_permanently_locked lock_name
           | [ Add_code ] -> is_permanently_locked Release
           | _ -> false
         in
         is_empty_umbrella_feature ()
         || is_permanently_locked_and_stable ()
       in
       let feature_ids_already_shown_in_the_own_table, owned =
         List.filter_map all_owned_feature_ids ~f:(fun feature_id ->
           if not include_all_owned_features
           && can_be_hidden (find_feature_by_id_exn t feature_id)
           then None
           else Some (feature_id, make_feature_info feature_id))
         |> List.unzip
         |> fun (ids, features) -> Feature_id.Hash_set.of_list ids, features
       in
       let watched =
         let watched_ids = Feature_id.Hash_set.create () in
         List.iter Features_by_parties.Parties.all ~f:(function
           | Owners -> () (* Treated separately above *)
           | Whole_feature_followers as parties ->
             Features_by_parties.find t.features_by_parties for_ parties
             |> List.iter ~f:(fun feature_id ->
               if not (Hash_set.mem feature_ids_already_shown_in_the_own_table feature_id)
               && not (can_be_hidden (find_feature_by_id_exn t feature_id))
               then Hash_set.add watched_ids feature_id
             ));
         List.map (Hash_set.to_list watched_ids) ~f:make_feature_info
       in
       let cr_soons =
         Cr_soons.get_all t.cr_soons ~for_or_all:(`User for_)
           ~include_active:include_active_cr_soons
       in
       let bookmarks_without_feature =
         match Hashtbl2_pair.find2 t.bookmarks_without_feature for_ with
         | None -> []
         | Some hashtbl -> Hashtbl.to_alist hashtbl
       in
       { Todo.Reaction.
         assigned
       ; unclean_workspaces
       ; owned
       ; watched
       ; cr_soons
       ; bookmarks_without_feature
       })
;;

let () =
  let module Unarchive_feature = Iron_protocol.Unarchive_feature in
  implement_deferred_rpc ~log:true Post_check_in_features.none
    (module Unarchive_feature)
    (fun t query ->
       let { Unarchive_feature.Action. feature_path; rev_zero; feature_id } =
         Query.action query
       in
       (if is_ok (find_feature t feature_path)
        then raise_s [%sexp "a feature with that name already exists"
                          , (feature_path : Feature_path.t)]);
       (match Feature_path.parent feature_path with
        | Error _ -> ()
        | Ok parent_path ->
          if is_error (find_feature t parent_path)
          then raise_s [%sexp "parent feature doesn't exist"
                            , (parent_path : Feature_path.t)]);
       let archived_feature =
         ok_exn (Archived_features.find_by_id t.archived_features feature_id)
       in
       let actual_feature_path = Archived_feature.feature_path archived_feature in
       if not (Feature_path.equal actual_feature_path feature_path)
       then
         raise_s
           [%sexp
             "feature_path mismatch",
             { feature_id                             : Feature_id.t
             ; actual_feature_path                    : Feature_path.t
             ; requested_feature_path = (feature_path : Feature_path.t)
             }
           ];
       let feature_rev_zero = Archived_feature.rev_zero archived_feature in
       if not (Rev.equal_node_hash rev_zero feature_rev_zero)
       then
         raise_s
           [%sexp
             "mismatch between feature family and local clone",
             { feature_path                 : Feature_path.t
             ; feature_rev_zero             : Rev.t
             ; local_rev_zero   = (rev_zero : Rev.t)
             }
           ];
       (* At this point, we've committed to unarchiving the feature.  The [uncompress] and
          [rename] steps are enqueued in the serializer sequencer so by the time they are
          executed the archived feature has made it to the disk.  For the same reason,
          [rename] is guaranteed not to start before the [uncompress] has finished. Also
          because we create consecutive jobs, we know that [fe show -archived] cannot run
          between our jobs and make us fail, for instance by recompressing the feature we
          just decompressed. *)
       Archived_features.remove t.archived_features query archived_feature;
       let archived_feature_dir = archived_feature_dir archived_feature in
       Serializer.uncompress_subtree_if_needed (serializer_exn t)
         ~dir:archived_feature_dir;
       Serializer.rename (serializer_exn t)
         ~from_:archived_feature_dir
         ~to_:  (feature_dir feature_id);
       let open Async in
       (* We need to make sure that the archived feature made it to disk.  Waiting for all
          prior changes is a crude way of doing so.  But it seems tricky and not worth the
          code to do something more clever, e.g. tracking for each archived feature
          whether or not it's on disk. *)
       let%bind () = prior_changes_synced_to_file_system t in
       let%map (feature, review_managers) =
         Deserializer.load
           (feature_deserializer ~dynamic_upgrade_state:t.dynamic_upgrade_state)
           ~root_directory:(Abspath.append
                              (Serializer.root_directory (serializer_exn t))
                              (feature_dir feature_id))
           ~serializer:(create_feature_serializer t feature_id)
       in
       install_deserialized_feature t feature review_managers;
       repartition_crs_by_assignee t feature;
       { Unarchive_feature.Reaction.
         feature_tip      = Feature.tip feature
       ; remote_repo_path = remote_repo_path t feature
       })
;;

let () =
  let module Unlock_feature = Iron_protocol.Unlock_feature in
  implement_rpc ~log:true
    (Post_check_in_features.one Unlock_feature.Action.feature_path)
    (module Unlock_feature)
    (fun t query ->
       let { Unlock_feature.Action. feature_path; for_; lock_names; even_if_permanent } =
         Query.action query
       in
       let feature = find_feature_exn t feature_path in
       List.map lock_names ~f:(fun lock_name ->
         lock_name, Feature.unlock feature ~query ~for_ ~lock_name ~even_if_permanent))
;;

let () =
  let module Update_bookmark = Iron_protocol.Update_bookmark in
  implement_rpc ~log:true
    ~override_sexp_of_action:[%sexp_of: Update_bookmark.Action.Concise.t]
    (Post_check_in_features.one_if_present Update_bookmark.Action.feature_path)
    (module Update_bookmark)
    (fun t query ->
       let { Update_bookmark.Action.
             feature_path
           ; info
           ; feature_id
           ; augment_worker_cache
           } =
         Query.action query
       in
       match find_feature t feature_path with
       | Error _ -> ()
       | Ok feature ->
         let race_during_update =
           Feature_id.(<>) (Feature.feature_id feature) feature_id
           || (match info with
             | Error _ -> false
             | Ok { base_facts; _ } ->
               Rev.Compare_by_hash.(<>)
                 (Rev_facts.rev base_facts) (Feature.base feature))
           || (match Feature.next_base_update feature with
             | No_update_expected -> false
             | Update_expected _  -> true)
         in
         if not race_during_update
         then (
           let result = Feature.update_bookmark feature query in
           (match info with
            | Error _ -> ()
            | Ok { crs_at_tip; cr_soons; tip_facts; _ } ->
              (match cr_soons with
               | Error _ -> ()
               | Ok cr_soons -> Cr_soons.update_feature t.cr_soons cr_soons);
              let crs_at_tip =
                match crs_at_tip with
                | Ok crs_at_tip -> crs_at_tip
                | Error _ ->
                  (* In this case, we don't know what the CR format is, so we don't know
                     what the CRs are.  By definition, the feature isn't CR clean.  We
                     clear out CRs because we don't know what they are. *)
                  assert (not (Feature.tip_is_cr_clean feature));
                  []
              in
              update_crs t feature crs_at_tip;
              update_review_manager_goals t feature;
              add_span_since_push_value_to_metric_if_available t ~feature
                ~tip:(tip_facts |> Rev_facts.rev |> Rev.to_first_12)
                ~metric_name:Metric_name.hydra_update_bookmark_latency);
           set_brains_to_goal_if_edge t query feature;
           let result = ok_exn result in
           (* If there was no error, we augment the worker cache *)
           if is_ok info then Worker_cache.augment t.worker_cache augment_worker_cache;
           result))
;;

let () =
  let module With_event_subscriptions = Iron_protocol.With_event_subscriptions in
  implement_rpc ~log:true Post_check_in_features.none
    (module With_event_subscriptions)
    (fun t query ->
       only_user_with_admin_privileges_is_authorized_exn t query;
       match Query.action query with
       | Set_max_subscriptions_per_user max ->
         Event_subscriptions.set_max_subscriptions_per_user t.event_subscriptions max
       | Set_max_subscriptions_global max ->
         Event_subscriptions.set_max_subscriptions_global t.event_subscriptions max
       | Drop_all_by_user user ->
         Event_subscriptions.drop_all_by_user t.event_subscriptions query user)
;;

let () =
  let module Notify_on_descendant_updates = Iron_protocol.Notify_on_descendant_updates in
  implement_pipe_rpc ~log:false
    (module Notify_on_descendant_updates)
    (fun t query ->
       let { Notify_on_descendant_updates.Action. feature_path; when_to_first_notify } =
         Query.action query
       in
       Feature_updates_manager.subscribe_feature_and_descendants
         t.feature_updates_manager
         query
         feature_path
         ~when_to_first_notify)
;;

let () =
  let module Notify_on_feature_updates = Iron_protocol.Notify_on_feature_updates in
  implement_pipe_rpc ~log:false
    (module Notify_on_feature_updates)
    (fun t query ->
       let open Async in
       let { Notify_on_feature_updates.Action. feature_id; when_to_first_notify } =
         Query.action query
       in
       let feature = find_feature_by_id_exn t feature_id in
       let pipe =
         Feature_updates_manager.subscribe_feature_only
           t.feature_updates_manager
           query
           feature_id
           ~when_to_first_notify
       in
       Pipe.map pipe ~f:(function
         | Ok `Updated -> Ok (`Updated (feature_to_protocol t feature))
         | (Ok `Archived | Error _ as res) -> res))
;;

let () =
  let module Notify_on_metric_updates
    = Iron_protocol.Notify_on_metric_updates in
  implement_pipe_rpc ~log:false
    (module Notify_on_metric_updates)
    (fun t query ->
       only_user_with_admin_privileges_is_authorized_exn t query;
       Metrics.subscribe t.metrics query (Query.action query))
;;

let rpc_implementations =
  let open Async in
  Rpc.Implementations.create_exn
    ~implementations:(Versioned_rpc.Menu.add !rpc_implementations)
    ~on_unknown_rpc:(`Call (fun _ ~rpc_tag ~version ->
      Log.Global.info "unknown rpc %s, version %d" rpc_tag version;
      `Close_connection))
;;
