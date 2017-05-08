module Stable = struct
  module Unstable = struct
    open! Core
    open! Import

    module Property = Property
  end

  open! Core.Core_stable
  open! Import_stable

  module Cr_soons_or_pending = struct
    module V1 = struct
      type t = Cr_soons.In_feature.V1.t Or_error.V1.t Or_pending.V1.t
      [@@deriving sexp]
    end
  end

  module Attributes = struct
    module V1 = struct
      type t =
        { feature_id       : Feature_id.V1.t
        ; feature_path     : Feature_path.V1.t
        ; owners           : User_name.V1.t list
        ; is_permanent     : bool
        ; description      : string
        ; base             : Rev.V1.t
        ; tip              : Rev.V1.t
        ; rev_zero         : Rev.V1.t
        ; remote_repo_path : Remote_repo_path.V1.t option
        }
      [@@deriving sexp]
    end
  end

  module Creation = struct
    module V1 = struct
      type t = Attributes.V1.t Query.V1.t
      [@@deriving sexp]
    end
  end

  module Bookmark_update = struct
    module V1 = struct
      type t =
        { base_facts              : Rev_facts.V1.t option
        ; tip                     : Rev.V1.t
        ; tip_facts               : Rev_facts.V1.t option
        ; base_is_ancestor_of_tip : Rev_facts.Is_ancestor.V1.t option
        }
      [@@deriving sexp]
    end
  end

  module Lock_name = struct
    module V2 = struct
      type t = Iron_common.Std.Lock_name.t =
        | Create_child
        | Rebase
        | Release
        | Release_into
        | Rename
        | Second
      [@@deriving sexp]
    end
  end

  module Lock = struct
    module V1 = struct
      type t =
        { lock_name     : Lock_name.V2.t
        ; for_          : User_name.V1.t
        ; reason        : string
        ; is_permanent  : bool [@default false]
        }
      [@@deriving sexp]
    end
  end

  module Unlock = struct
    module V1 = struct
      type t =
        { lock_name          : Lock_name.V2.t
        ; for_               : User_name.V1.t
        ; even_if_permanent  : bool [@default false]
        }
      [@@deriving sexp]
    end
  end

  module Remove_properties = struct
    module V1 = struct
      type t = Property.V1.Set.t [@@deriving sexp_of]

      (* Remove_properties used to take Property.t list. The t_of_sexp function below
         ensures that the server won't choke on old persistency records that contain
         duplicates. *)
      let t_of_sexp sexp =
        sexp
        |> [%of_sexp: Property.V1.t list]
        |> Unstable.Property.Set.of_list
      ;;
    end
  end

  module Action = struct
    module V1 = struct
      (* [Ask_hydra_to_retry] and [Set_force_hydra_retry] are no longer used.  We no
         longer persist [bounded_hydra_retry].  This can cause a little bit of extra
         retrying after a server bounce, for features that Iron hydra can't process, but
         that is small. *)
      type t =
        [ `Ask_hydra_to_retry of [ `Done | `Pending_or_working_on_it ]
        | `Lock                                    of Lock.V1.t
        | `Remove_properties                       of Remove_properties.V1.t
        | `Remove_inheritable_properties           of Property.V1.Set.t
        | `Set_base                                of Rev.V1.t
        | `Set_crs_are_enabled                     of bool
        | `Set_crs_shown_in_todo_only_for_users_reviewing of bool
        | `Set_feature_path                        of Feature_path.V1.t
        | `Set_description                         of string
        | `Set_force_hydra_retry
        | `Set_has_bookmark                        of bool
        | `Set_inheritable_crs_shown_in_todo_only_for_users_reviewing  of bool option
        | `Set_inheritable_xcrs_shown_in_todo_only_for_users_reviewing of bool option
        | `Set_inheritable_owners                  of User_name.V1.t list
        | `Set_inheritable_properties              of Properties.V1.t
        | `Set_inheritable_release_process         of Release_process.V1.t option
        | `Set_inheritable_who_can_release_into_me of Who_can_release_into_me.V1.t option
        | `Set_inheritable_send_email_to           of Email_address.V1.Set.t
        | `Set_inheritable_send_email_upon         of Send_email_upon.V1.Set.t
        | `Set_inheritable_whole_feature_followers of User_name.V1.Set.t
        | `Set_inheritable_whole_feature_reviewers of User_name.V1.Set.t
        | `Set_is_permanent                        of bool
        | `Set_lines_required_to_separate_ddiff_hunks of int
        | `Set_owners                              of User_name.V1.t list
        | `Set_properties                          of Properties.V1.t
        | `Set_release_process                     of Release_process.V1.t
        | `Set_review_is_enabled                   of bool
        | `Set_reviewing                           of Reviewing.V1.t
        | `Set_seconder                            of User_name.V1.t option
        | `Set_send_email_to                       of Email_address.V1.Set.t
        | `Set_send_email_upon                     of Send_email_upon.V1.Set.t
        | `Set_send_release_email_to               of Email_address.V1.Set.t
        | `Set_should_send_release_email           of bool
        | `Set_who_can_release_into_me             of Who_can_release_into_me.V1.t
        | `Set_whole_feature_followers             of User_name.V1.Set.t
        | `Set_whole_feature_reviewers             of User_name.V1.Set.t
        | `Set_xcrs_shown_in_todo_only_for_users_reviewing of bool
        | `Unlock                                  of Unlock.V1.t
        | `Update_bookmark                         of Bookmark_update.V1.t Or_error.V1.t
        ]
      [@@deriving sexp]
    end
  end

  module Action_query = struct
    module V1 = struct
      type t = Action.V1.t Query.V1.t
      [@@deriving sexp]
    end
  end

  module Diff_from_base_to_tip = struct
    module V2 = struct
      type t = Diff2s.V2.t Or_error.V1.t Or_pending.V1.t
      [@@deriving sexp]
    end
  end
end

open! Core
open! Import

module Action = struct
  include Stable.Action.V1
  let invariant (_ : t) = ()
end

module Attributes           = Stable.Attributes.           V1
module Bookmark_update      = Stable.Bookmark_update.      V1
module Cr_soons_or_pending  = Stable.Cr_soons_or_pending.  V1
module Creation             = Stable.Creation.             V1
module Action_query         = Stable.Action_query.         V1

module Diff_from_base_to_tip = struct
  include Stable.Diff_from_base_to_tip.V2
  let invariant t =
    Or_pending.invariant (Or_error.invariant Diff2s.invariant) t
  ;;
end
module Lock = Stable.Lock.V1
module Unlock = Stable.Unlock.V1

module Persist = struct
  module Cr_soons_or_pending = struct
    include Persistent.Make
        (struct let version = 1 end)
        (Stable.Cr_soons_or_pending.V1)
  end
  module Creation = struct
    include Persistent.Make
        (struct let version = 1 end)
        (Stable.Creation.V1)
  end
  module Action_query = struct
    include Persistent.Make
        (struct let version = 1 end)
        (Stable.Action_query.V1)
  end
  module Diff_from_base_to_tip = struct
    include Persistent.Make
        (struct let version = 1 end)
        (Stable.Diff_from_base_to_tip.V2)
  end
  module Diff4s_file = struct
    module V2 = struct
      open Core.Core_stable
      open Import_stable
      type t = Diff4.V2.t list Or_error.V1.t Or_pending.V1.t
      [@@deriving sexp]
    end
    include Persistent.Make
        (struct let version = 1 end)
        (V2)
  end
  module Allow_review_for = struct
    include Persistent.Make
        (struct let version = 1 end)
        (Allow_review_for.Stable.V1)
  end
  module Included_feature = struct
    include Persistent.Make
        (struct let version = 3 end)
        (Released_feature.Stable.V3)
    include Register_read_old_version
        (struct let version = 2 end)
        (Released_feature.Stable.V2)
    include Register_read_old_version
        (struct let version = 1 end)
        (Released_feature.Stable.V1)
  end
  module Latest_release = struct
    include Persistent.Make
        (struct let version = 1 end)
        (Latest_release.Stable.V1)
  end
end

module Hydra_master_state = struct
  (* There are two different kinds of rpcs Iron receives from hydra, the ones coming from
     the master (synchronize-state), and the ones coming from the workers
     (update-bookmark).  This represents the most current state of the hydra master we
     know for a bookmark. *)
  type t =
    { tip                                 : Node_hash.First_12.t
    ; status                              : [ `Done | `Pending_or_working_on_it ]
    ; override_n_pending_status_if_needed : int
    }
  [@@deriving compare, sexp_of]
end

module Next_base_update_expected = struct
  type t =
    { update_expected : Next_base_update.Update_expected.t
    ; expiration      : Timed_event.t
    }
  [@@deriving fields, sexp_of]
end

module Feature_description : sig
  type t [@@deriving sexp_of]
  include Stringable.S with type t := t
end = struct
  type t = string [@@deriving sexp_of]
  let to_string t = t
  let of_string string = String.strip string
end

type t =
  (* The fields are ordered for readability of [sexp_of_t]. *)
  { mutable feature_path              : Feature_path.t
  ; feature_id                        : Feature_id.t
  (* Unassigned CRs are assigned to the first user in [owners]. *)
  ; mutable owners                    : User_name.t list
  ; mutable description               : Feature_description.t
  ; mutable whole_feature_followers   : User_name.Set.t
  ; mutable whole_feature_reviewers   : User_name.Set.t
  ; mutable seconder                  : User_name.t option
  ; mutable num_lines                 : int Or_error.t Or_pending.t
  ; mutable base                      : Rev.t
  ; mutable next_base_update_expected : Next_base_update_expected.t option
  ; mutable tip                       : Rev.t
  (* The latest update-bookmark we received, or [Ok ()] before we received any. *)
  ; mutable latest_bookmark_update    : unit Or_error.t
  ; mutable next_bookmark_update      : Next_bookmark_update.t
  (* [hydra_master_state] is [None] only for initialization, which is treated as
     up-to-date to avoid having things pending everywhere in the tests.  It doesn't matter
     in prod, because this field will be filled in a few seconds after startup by a
     synchronize-state RPC. *)
  ; mutable hydra_master_state        : Hydra_master_state.t option
  ; mutable has_bookmark              : bool
  ; mutable compilation_status : Compilation_status.one Repo_controller_name.Table.t
  ; mutable continuous_release_status : Continuous_release_status.t
  ; mutable is_permanent              : bool
  ; mutable release_process           : Release_process.t
  ; mutable review_is_enabled         : bool
  ; mutable reviewing                 : Reviewing.t
  ; mutable crs_are_enabled           : bool
  ; mutable crs_shown_in_todo_only_for_users_reviewing  : bool
  ; mutable xcrs_shown_in_todo_only_for_users_reviewing : bool
  ; mutable who_can_release_into_me   : Who_can_release_into_me.t
  ; mutable send_email_to             : Email_address.Set.t
  ; mutable send_email_upon           : Send_email_upon.Set.t
  ; mutable base_is_ancestor_of_tip   : Rev_facts.Is_ancestor.t Or_pending.t
  ; mutable base_facts                : Rev_facts.t Or_pending.t
  ; mutable tip_facts                 : Rev_facts.t Or_pending.t
  ; allow_review_for : (Allow_review_for.t, read_write) Ref.Permissioned.t
  ; mutable dynamic_upgrade_state     : Dynamic_upgrade.State.t
  ; mutable serializer                : Serializer.t option
  (* [diff_from_base_to_tip] is the *entire* diff, including even things that
     whole-feature reviewers and file reviewers never see.  This is useful to support
     the [-include-ignored] switch for "fe diff". *)
  ; mutable diff_from_base_to_tip     : Diff_from_base_to_tip.t
  ; mutable indexed_diff4s            : Indexed_diff4s.t Or_error.t Or_pending.t
  ; mutable bounded_hydra_retry       : Bounded_hydra_retry.t
  ; creation                          : Creation.t
  ; locks                             : Feature_locks.t
  (* We have a single [cache_invalidator] for each feature, and store it in all structures
     that comprise the state of the feature, e.g. review manager, review sessions.
     [next_steps] is a cached computation that depends on the state of this feature as
     well as its parent, which is expressed by making the [Cached.t] depend on the
     [cache_invalidator] of the feature and its parent.  We invalidate feature
     cache-invalidators in multiple ways:

     - via serializer operations in features, review managers, and review sessions.
     - in state.ml via a [Changes_feature] declaration for each RPC that states what
     features the RPC changes
     - ad-hoc code not covered by the above, e.g. the handling of the synchronize-state
     RPC.
  *)
  ; cache_invalidator                 : Cached.Invalidator.t
  ; mutable line_count_by_user_cached : Line_count.Cached_in_feature.By_user.t Cached.t
  ; mutable next_steps_cached         : Next_step.t list Cached.t
  ; mutable review_analysis_cached    : Review_analysis.t option Cached.t
  ; mutable cr_soons                  : Cr_soons.In_feature.t Or_error.t Or_pending.t
  ; mutable included_features         : Released_feature.t list
  (* [queries] is a reverse-chron list of all calls to [query]. *)
  ; mutable queries                   : Action_query.t list
  ; properties                        : Sexp.t Property.Table.t
  ; mutable latest_release            : Latest_release.t option
  ; mutable inheritable_attributes    : Feature_inheritable_attributes.t
  ; mutable lines_required_to_separate_ddiff_hunks : int option
  }
[@@deriving fields, sexp_of]

let description t = Feature_description.to_string t.description

let rev_zero (t : t) = (Query.action t.creation).rev_zero
let remote_repo_path (t : t) = (Query.action t.creation).remote_repo_path

let invalidate_dependents t = Cached.Invalidator.invalidate_dependents t.cache_invalidator

let clear_cached_attributes t =
  Cached.clear t.line_count_by_user_cached;
  Cached.clear t.next_steps_cached;
  Cached.clear t.review_analysis_cached;
;;

let check_cached_attributes t ~ignore_diffs_in_errors =
  let field f = (Cached.T (Field.get f t), Field.name f) in
  [ field Fields.line_count_by_user_cached
  ; field Fields.next_steps_cached
  ; field Fields.review_analysis_cached
  ]
  |> List.map ~f:(fun (Cached.T cached, tag) ->
    Or_error.tag (Cached.check cached ~ignore_diffs_in_errors) ~tag:tag)
  |> Or_error.combine_errors_unit
;;

let set_cached_attributes t ~line_count_by_user ~next_steps ~review_analysis =
  (* The cached attributes are not supposed to be set after initialization, so the
     invalidation is not necessary.  But there is no guarantee about that fact.  We could
     consider using [Set_once.t] for the [*_cached] fields, which would force us to commit
     to this invariant (the invariant is not necessary right now).  Another idea is to use
     an immutable value for these fields, but that would not be easy because the
     computation closure inside the cached attribute depends on the [Feature.t] value
     itself (rec) and thus cannot be built before the feature. *)
  invalidate_dependents t;
  t.line_count_by_user_cached <- line_count_by_user;
  t.next_steps_cached         <- next_steps;
  t.review_analysis_cached    <- review_analysis;
;;

let line_count_by_user t = Cached.get t.line_count_by_user_cached
let next_steps t = ok_exn (Cached.get t.next_steps_cached)
let review_analysis t = ok_exn (Cached.get t.review_analysis_cached)

let allow_review_for t = Ref.Permissioned.read_only t.allow_review_for

let users_with_review_session_in_progress t =
  match review_analysis t with
  | exception exn -> Or_error.of_exn exn
  | None ->
    Ok User_name.Set.empty
  | Some review_analysis ->
    Ok (Review_analysis.users_with_review_session_in_progress review_analysis)
;;

let next_base_update t =
  match t.next_base_update_expected with
  | None -> Next_base_update.No_update_expected
  | Some { update_expected; expiration = _ } -> Update_expected update_expected
;;

let find_property t property = Hashtbl.find t.properties property
;;

let properties t = Property.Map.of_hashtbl_exn t.properties
;;

let to_protocol t
      ~is_archived
      ~is_rebased
      ~remote_repo_path
      ~has_children
      ~cr_summary
      ~line_count_by_user
      ~users_with_unclean_workspaces
  =
  { Iron_protocol.Feature.
    feature_id = t.feature_id
  ; feature_path = t.feature_path
  ; rev_zero = rev_zero t
  ; owners = t.owners
  ; whole_feature_followers = t.whole_feature_followers
  ; whole_feature_reviewers = t.whole_feature_reviewers
  ; base = t.base
  ; base_facts = t.base_facts
  ; next_base_update = next_base_update t
  ; next_bookmark_update = t.next_bookmark_update
  ; has_bookmark = t.has_bookmark
  ; tip = t.tip
  ; tip_facts = t.tip_facts
  ; base_is_ancestor_of_tip = t.base_is_ancestor_of_tip
  ; diff_from_base_to_tip = t.diff_from_base_to_tip
  ; description = description t
  ; is_permanent = t.is_permanent
  ; review_is_enabled = t.review_is_enabled
  ; crs_are_enabled = t.crs_are_enabled
  ; crs_shown_in_todo_only_for_users_reviewing
    = t.crs_shown_in_todo_only_for_users_reviewing
  ; xcrs_shown_in_todo_only_for_users_reviewing
    = t.xcrs_shown_in_todo_only_for_users_reviewing
  ; reviewing = t.reviewing
  ; allow_review_for = Ref.Permissioned.get t.allow_review_for
  ; seconder = t.seconder
  ; included_features = List.rev t.included_features
  ; properties = properties t
  ; has_children
  ; release_process = t.release_process
  ; who_can_release_into_me = t.who_can_release_into_me
  ; send_email_to = t.send_email_to
  ; send_email_upon = t.send_email_upon
  ; remote_repo_path
  ; locked = Feature_locks.what_is_locked t.locks
  ; line_count_by_user
  ; cr_summary
  ; next_steps                = next_steps t
  ; users_with_review_session_in_progress = users_with_review_session_in_progress t
  ; users_with_unclean_workspaces
  ; is_archived
  ; is_rebased
  ; latest_release = t.latest_release
  ; inheritable_attributes
    = Feature_inheritable_attributes.to_protocol t.inheritable_attributes
  ; compilation_status = Repo_controller_name.Map.of_hashtbl_exn t.compilation_status
  }
;;

let to_released_feature_exn t ~query ~tagged_tip =
  let tip =
    match tagged_tip with
    | None -> t.tip
    | Some tagged_tip ->
      if not (Rev.equal_node_hash t.tip tagged_tip)
      then
        raise_s
          [%sexp
            "Feature.to_released_feature_exn got mismatched tagged_tip"
          , { feature_tip = (t.tip : Rev.t)
            ; tagged_tip           : Rev.t
            }
          ];
      tagged_tip
  in
  let seconder =
    match t.seconder with
    | Some seconder -> seconder
    | None -> failwith "feature is not seconded"
  in
  { Released_feature.
    feature_id              = t.feature_id
  ; feature_path            = t.feature_path
  ; description             = description t
  ; owners                  = t.owners
  ; whole_feature_followers = t.whole_feature_followers
  ; whole_feature_reviewers = t.whole_feature_reviewers
  ; seconder
  ; base                    = t.base
  ; tip
  ; properties              = properties t
  ; includes                = List.rev t.included_features
  ; release_cause           = Query.with_action query ()
  }
;;

(* Because this function reads [t.cr_soons] and [t.diff_from_base_to_tip] which don't have
   proper values while replaying queries, this function and its transitive callers should
   not be called either while replaying queries (or if they are, any value computed from
   them should be recomputed after filling in these two fields, like we do with
   [t.expecting_bookmark_update_since]). *)
let needs_bookmark_update t =
  false
  || is_pending t.base_is_ancestor_of_tip
  || is_pending t.base_facts
  || is_pending t.tip_facts
  || is_pending t.cr_soons
  || is_pending t.diff_from_base_to_tip
  || (match t.hydra_master_state with
    | None -> false
    | Some hydra_master_state ->
      not (Rev.has_prefix t.tip hydra_master_state.tip))
;;

let would_ask_for_a_bookmark_update t =
  match Bounded_hydra_retry.should_force t.bounded_hydra_retry with
  | Some _ as opt -> opt
  | None ->
    match t.hydra_master_state with
    | Some { status = `Pending_or_working_on_it; _ } -> None
    | None | Some { status = `Done; _ } ->
      match t.latest_bookmark_update with
      | Error _ ->
        (* If hydra already reported an error, we don't ask it again. *)
        None
      | Ok _ ->
        if needs_bookmark_update t
        then Bounded_hydra_retry.can_retry t.bounded_hydra_retry
        else None
;;

let compute_next_bookmark_update t =
  let am_expecting_bookmark_update =
    match t.hydra_master_state with
    | Some { status = `Pending_or_working_on_it; _ } -> true
    | None | Some { status = `Done; _ } ->
      Option.is_some (would_ask_for_a_bookmark_update t : Bounded_hydra_retry.t option)
  in
  if am_expecting_bookmark_update
  then (
    match t.next_bookmark_update with
    | Update_expected_since _ as x -> x
    | No_update_expected | No_update_expected_due_to_iron_bug _ ->
      Update_expected_since (Time.now ()))
  else if not (needs_bookmark_update t)
  then No_update_expected
  else (
    match t.latest_bookmark_update with
    | Error error -> No_update_expected_due_to_iron_bug error
    | Ok () ->
      No_update_expected_due_to_iron_bug
        (Error.of_string "Iron was unable to process this feature"))
;;

let refresh_next_bookmark_update t =
  let new_value = compute_next_bookmark_update t in
  if Next_bookmark_update.compare t.next_bookmark_update new_value <> 0
  then (
    invalidate_dependents t;
    t.next_bookmark_update <- new_value)
;;

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~feature_id:ignore
      ~feature_path:(check (fun feature_path ->
        Feature_path.invariant feature_path;
        [%test_result: bool] (is_some (remote_repo_path t))
          ~expect:(Feature_path.is_root feature_path)))
      ~locks:(check Feature_locks.invariant)
      ~cache_invalidator:(check Cached.Invalidator.invariant)
      ~line_count_by_user_cached:
        (check (Cached.invariant Line_count.Cached_in_feature.By_user.invariant))
      ~next_steps_cached:(check (Cached.invariant ignore))
      ~review_analysis_cached:
        (check (Cached.invariant (Option.invariant Review_analysis.invariant)))
      ~owners:(check (fun owners ->
        assert (not (List.is_empty owners));
        List.iter owners ~f:User_name.invariant))
      ~whole_feature_followers:(check (Set.iter ~f:User_name.invariant))
      ~whole_feature_reviewers:(check (Set.iter ~f:User_name.invariant))
      ~seconder:(check (function
        | None -> ()
        | Some user -> User_name.invariant user))
      ~base:(check Rev.invariant)
      ~next_base_update_expected:(check (Option.iter ~f:(function
        | { Next_base_update_expected.
            update_expected = { rev; by = _; expected_since = _ } as update_expected
          ; expiration
          } ->
          Timed_event.invariant expiration;
          Next_base_update.Update_expected.invariant update_expected;
          assert (not (Rev.Compare_by_hash.equal rev t.base)))))
      ~base_facts:
        (check (Or_pending.invariant (fun base_facts ->
           Rev_facts.invariant base_facts;
           assert (Rev.equal_node_hash t.base (Rev_facts.rev base_facts)))))
      ~latest_bookmark_update:ignore
      ~next_bookmark_update:(check (fun next_bookmark_update ->
        assert (Next_bookmark_update.same_variant next_bookmark_update
                  (compute_next_bookmark_update t))))
      ~hydra_master_state:ignore
      ~has_bookmark:ignore
      ~compilation_status:(check (Hashtbl.iter ~f:Compilation_status.invariant))
      ~continuous_release_status:(check ignore)
      ~tip:(check Rev.invariant)
      ~tip_facts:
        (check (Or_pending.invariant (fun tip_facts ->
           Rev_facts.invariant tip_facts;
           assert (Rev.equal_node_hash t.tip (Rev_facts.rev tip_facts)))))
      ~base_is_ancestor_of_tip:
        (check (Or_pending.invariant (fun base_is_ancestor_of_tip ->
           assert (is_ok (Rev_facts.Is_ancestor.check base_is_ancestor_of_tip
                            ~ancestor:t.base ~descendant:t.tip)))))
      ~diff_from_base_to_tip:(check Diff_from_base_to_tip.invariant)
      ~indexed_diff4s:(check (Or_pending.invariant
                                (Or_error.invariant Indexed_diff4s.invariant)))
      ~is_permanent:ignore
      ~review_is_enabled:ignore
      ~reviewing:(check (function
        | `All | `Whole_feature_reviewers -> ()
        | `All_but users | `Only users -> Set.iter users ~f:User_name.invariant))
      ~allow_review_for:ignore
      ~crs_are_enabled:ignore
      ~crs_shown_in_todo_only_for_users_reviewing:ignore
      ~xcrs_shown_in_todo_only_for_users_reviewing:ignore
      ~description:ignore
      ~bounded_hydra_retry:(check Bounded_hydra_retry.invariant)
      ~creation:ignore
      ~included_features:(check (List.iter ~f:Released_feature.invariant))
      ~queries:(check (fun queries ->
        List.iter queries ~f:(Query.invariant Action.invariant)))
      ~dynamic_upgrade_state:(check Dynamic_upgrade.State.Reference.invariant)
      ~serializer:(check (Option.iter ~f:Serializer.invariant))
      ~cr_soons:
        (check (Or_pending.invariant (Or_error.invariant Cr_soons.In_feature.invariant)))
      ~num_lines:(check (Or_pending.invariant
                           (Or_error.invariant (fun num_lines ->
                              assert (num_lines >= 0)))))
      ~properties:ignore
      ~release_process:ignore
      ~who_can_release_into_me:ignore
      ~send_email_to:ignore
      ~send_email_upon:ignore
      ~latest_release:(check (Option.iter ~f:Latest_release.invariant))
      ~inheritable_attributes:(check Feature_inheritable_attributes.invariant)
      ~lines_required_to_separate_ddiff_hunks:(check (function
        | None -> ()
        | Some num_lines ->
          assert (Feature_path.is_root t.feature_path);
          assert (num_lines >= 0))))
;;

let serializer_exn t =
  match t.serializer with
  | Some s -> s
  | None ->
    raise_s [%sexp "serializer isn't defined", (t : t), (Backtrace.get () : Backtrace.t)]
;;

let first_owner t =
  match t.owners with
  | owner :: _ -> owner
  | [] ->
    raise_s [%sexp "Feature.first_owner encountered feature with no owners", (t : t)]
;;

let is_empty t = Rev.equal_node_hash t.base t.tip

let is_owner t user = List.mem t.owners user ~equal:User_name.equal

let all_whole_feature_reviewers_are_owners t =
  Set.for_all (whole_feature_reviewers t) ~f:(fun whole_feature_reviewer ->
    is_owner t whole_feature_reviewer)
;;

let is_seconded t = is_some t.seconder

let tip_is_cr_clean t =
  match t.tip_facts with
  | Pending_since _ -> false
  | Known facts ->
    match Rev_facts.Is_cr_clean.check facts.is_cr_clean t.tip with
    | Ok b -> b
    | Error _ -> false
;;

let user_is_currently_reviewing t user =
  t.review_is_enabled
  && Reviewing.mem t.reviewing user
       ~whole_feature_reviewers:t.whole_feature_reviewers
       ~whole_feature_followers:t.whole_feature_followers
       ~is_seconded:(Option.is_some t.seconder)
;;

let allow_review_for_file = Relpath.of_string "allow-review-for"
let cr_soons_file = Relpath.of_string "cr-soons"
let creation_file = Relpath.of_string "creation"
let diff_from_base_to_tip_file = Relpath.of_string "diff-from-base-to-tip"
let diff4s_file = Relpath.of_string "diff4s"
let included_features_file = Relpath.of_string "included-features"
let queries_file = Relpath.of_string "queries"
let latest_release_file = Relpath.of_string "latest-release"

let set_diffs_internal t diff_from_base_to_tip diff4s =
  t.diff_from_base_to_tip <- diff_from_base_to_tip;
  t.indexed_diff4s <-
    Or_pending.map diff4s ~f:(Or_error.map ~f:Indexed_diff4s.create);
  t.num_lines <-
    Or_pending.map diff_from_base_to_tip
      ~f:(Or_error.map
            ~f:(fun diff ->
              Diff2s.num_lines_for_feature_size diff));
;;

let set_diffs t diff_from_base_to_tip diff4s =
  set_diffs_internal t diff_from_base_to_tip diff4s;
  let serializer = serializer_exn t in
  Serializer.set_contents serializer ~file:diff_from_base_to_tip_file
    diff_from_base_to_tip (module Persist.Diff_from_base_to_tip);
  Serializer.set_contents serializer ~file:diff4s_file
    diff4s (module Persist.Diff4s_file);
;;

let set_allow_review_for_internal t allow_review_for =
  Ref.Permissioned.set t.allow_review_for allow_review_for;
;;

let set_allow_review_for t allow_review_for =
  set_allow_review_for_internal t allow_review_for;
  Serializer.set_contents (serializer_exn t) ~file:allow_review_for_file
    allow_review_for (module Persist.Allow_review_for);
;;

let set_cr_soons_internal t cr_soons =
  t.cr_soons <- cr_soons;
;;

let set_cr_soons t cr_soons =
  set_cr_soons_internal t cr_soons;
  Serializer.set_contents (serializer_exn t) ~file:cr_soons_file
    cr_soons (module Persist.Cr_soons_or_pending);
;;

let set_latest_release_internal t latest_release =
  t.latest_release <- Some latest_release
;;

let set_latest_release t latest_release =
  set_latest_release_internal t latest_release;
  Serializer.set_contents (serializer_exn t) ~file:latest_release_file
    latest_release (module Persist.Latest_release);
;;

let create_internal creation ~dynamic_upgrade_state ~serializer =
  let { Attributes.
        feature_id
      ; feature_path
      ; owners
      ; is_permanent
      ; description
      ; base
      ; tip
      ; rev_zero = _
      ; remote_repo_path = _
      } = Query.action creation
  in
  let pending = pending_now () in
  { feature_id
  ; feature_path
  ; owners
  ; whole_feature_followers = User_name.Set.empty
  ; whole_feature_reviewers = User_name.Set.of_list owners
  ; seconder = None
  ; base
  ; next_base_update_expected = None
  ; base_facts = pending
  ; latest_bookmark_update = Ok ()
  ; next_bookmark_update = Update_expected_since (Time.now ())
  ; hydra_master_state = None
  ; has_bookmark = true
  ; compilation_status = Repo_controller_name.Table.create ()
  ; continuous_release_status = `Not_working_on_it
  ; tip
  ; tip_facts = pending
  ; base_is_ancestor_of_tip = pending
  ; diff_from_base_to_tip = pending
  ; indexed_diff4s = pending
  ; is_permanent
  ; review_is_enabled = false
  ; reviewing =
      `Whole_feature_reviewers
  ; allow_review_for = Ref.Permissioned.create Allow_review_for.none
  ; crs_are_enabled = true
  ; crs_shown_in_todo_only_for_users_reviewing
    = Iron_protocol.Feature.Default_values.crs_shown_in_todo_only_for_users_reviewing
  ; xcrs_shown_in_todo_only_for_users_reviewing
    = Iron_protocol.Feature.Default_values.xcrs_shown_in_todo_only_for_users_reviewing
  ; description = Feature_description.of_string description
  ; num_lines = pending
  ; bounded_hydra_retry = Bounded_hydra_retry.empty
  ; creation
  ; locks = Feature_locks.create feature_path
  ; cache_invalidator =
      Cached.Invalidator.create
        ~debug_information:[%sexp `Feature (feature_id : Feature_id.t)]
  ; line_count_by_user_cached = Cached.uninitialized ~name:"Feature.line_count_by_user" ()
  ; next_steps_cached = Cached.uninitialized ~name:"Feature.next_steps" ()
  ; review_analysis_cached = Cached.uninitialized ~name:"Feature.review_analysis" ()
  ; included_features = []
  ; queries = []
  ; dynamic_upgrade_state
  ; serializer
  ; cr_soons = pending
  ; properties = Property.Table.create ()
  ; release_process = Direct
  ; who_can_release_into_me = My_owners
  ; send_email_to = Email_address.Set.empty
  ; send_email_upon = Send_email_upon.default
  ; latest_release = None
  ; inheritable_attributes = Feature_inheritable_attributes.create ()
  ; lines_required_to_separate_ddiff_hunks = None
  }
;;

let create_exn query
      ~feature_id ~feature_path ~owners ~is_permanent ~description ~base ~tip ~rev_zero
      ~remote_repo_path ~dynamic_upgrade_state ~serializer =
  if List.is_empty owners
  then failwith "feature must have an owner"
  else if is_some remote_repo_path && not (Feature_path.is_root feature_path)
  then failwith "only root features can specify a remote repo path"
  else if is_none remote_repo_path && Feature_path.is_root feature_path
  then failwith "a root feature must specify a remote repo path"
  else (
    let creation =
      Query.with_action query
        { Attributes.
          feature_id
        ; feature_path
        ; owners
        ; is_permanent
        ; description
        ; base
        ; tip
        ; rev_zero
        ; remote_repo_path
        }
    in
    let serializer = force serializer in
    let t =
      create_internal creation ~dynamic_upgrade_state
        ~serializer:(Some serializer)
    in
    Serializer.add_cache_invalidator serializer t.cache_invalidator;
    set_cr_soons t t.cr_soons;
    let pending = pending_now () in
    set_diffs t pending pending;
    Serializer.set_contents serializer
      ~file:creation_file creation (module Persist.Creation);
    t)
;;

let review_goal t =
  match
    t.base_facts,
    t.tip_facts,
    t.base_is_ancestor_of_tip,
    t.diff_from_base_to_tip
  with
  | Known base_facts,
    Known tip_facts,
    Known base_is_ancestor_of_tip,
    Known diff_from_base_to_tip ->
    Or_error.bind diff_from_base_to_tip ~f:(fun diff_from_base_to_tip ->
      Review_goal.create diff_from_base_to_tip
        ~base_facts ~tip_facts ~base_is_ancestor_of_tip)
  | _ ->
    error_string "no review goal because the feature is broken or pending"
;;

let record t query action =
  let allowed_from =
    match (action : Action.t) with
    | `Lock   { lock_name = (Create_child | Second); _ }
    | `Unlock { lock_name = (Create_child | Second); _ } -> Dynamic_upgrade.U2
    (* Note to devs: please keep at least this line so that it is easy to add/remove
       lines there *)
    | #Action.t -> Dynamic_upgrade.U1
  in
  match Dynamic_upgrade.commit_to_upgrade t.dynamic_upgrade_state ~allowed_from with
  | `Not_allowed_yet ->
    (* Make sure to invalidate the cache in the case where the event are not recorded.
       This is handled by the serializer directly in the other case. *)
    invalidate_dependents t
  | `Ok ->
    let query = Query.with_action query action in
    t.queries <- query :: t.queries;
    match t.serializer with
    | None ->
      (* Here and anytime t.serializer is None, the cached values are uninitialized
         (because they are set outside of feature.ml, and feature.ml always returns
         features with serializers). So anyone depending on cached fields during
         initialization of a feature would get an exception, and we don't need to
         invalidate anything. *)
      ()
    | Some serializer ->
      Serializer.append_to serializer ~file:queries_file query
        (module Persist.Action_query)
;;

let include_released_feature t released_feature =
  Serializer.append_to (serializer_exn t)
    ~file:included_features_file released_feature (module Persist.Included_feature);
  t.included_features <- released_feature :: t.included_features;
;;

let clear_included_features t =
  Serializer.clear_sequence (serializer_exn t) ~file:included_features_file;
  t.included_features <- [];
;;

let set_next_base_update_internal t (value : Next_base_update.t) =
  (match t.next_base_update_expected with
   | None -> ()
   | Some { update_expected = _; expiration } ->
     Timed_event.abort_if_possible expiration);
  let next_base_update_expected =
    match value with
    | No_update_expected -> None
    | Update_expected update_expected ->
      let expiration =
        Timed_event.run (`After (Time.Span.of_min 30.))
          (Next_base_update_expiration { feature_id = t.feature_id })
      in
      Some { Next_base_update_expected. update_expected; expiration }
  in
  t.next_base_update_expected <- next_base_update_expected;
;;

let expect_next_base_update_exn t ~for_ rev =
  if Rev.Compare_by_hash.equal rev t.base
  then
    raise_s
      [%sexp "invalid expected next base.  This is already the feature's base"
           , (rev : Rev.t)
      ]
  else (
    set_next_base_update_internal t
      (Update_expected
         { rev
         ; by = for_
         ; expected_since = Time.now ()
         });
    invalidate_dependents t)
;;

let fire_next_base_update_expiration_if_applicable t ~expiration_id =
  match t.next_base_update_expected with
  | None -> ()
  | Some { update_expected = _; expiration } ->
    if Timed_event.has_id expiration expiration_id
    then (
      t.next_base_update_expected <- None;
      t.bounded_hydra_retry <- Bounded_hydra_retry.empty;
      invalidate_dependents t;
      refresh_next_bookmark_update t)
;;

let set_base t query rev =
  (* We can't trust the human_readable name of the rev to be stable, since it can come
     from a user query it may have an arbitrary name.  So we drop it.  Instead we'll get
     the tagged based when the next update bookmark hits. *)
  record t query (`Set_base rev);
  t.base <- Rev.without_human_readable rev;
  set_next_base_update_internal t No_update_expected;
  t.bounded_hydra_retry <- Bounded_hydra_retry.empty;
  let pending = pending_now () in
  t.base_facts <- pending;
  t.base_is_ancestor_of_tip <- pending;
  if Option.is_some t.serializer
  then set_diffs t pending pending
  else set_diffs_internal t pending pending;
  refresh_next_bookmark_update t;
;;

let set_continuous_release_status t status =
  if not (Continuous_release_status.equal t.continuous_release_status status)
  then (
    invalidate_dependents t;
    t.continuous_release_status <- status)
;;

let set_feature_path t query feature_path =
  record t query (`Set_feature_path feature_path);
  t.feature_path <- feature_path;
  Feature_locks.set_feature_path t.locks feature_path;
;;

let set_has_bookmark_internal t query has_bookmark =
  if not (Bool.equal t.has_bookmark has_bookmark)
  then (
    record t query (`Set_has_bookmark has_bookmark);
    t.has_bookmark <- has_bookmark)
;;

let set_has_no_bookmark t query =
  set_continuous_release_status t `Not_working_on_it;
  set_has_bookmark_internal t query false
;;

let set_has_bookmark t query ~compilation_status =
  let had_bookmark_before = t.has_bookmark in
  set_has_bookmark_internal t query true;
  match compilation_status with
  | `Keep_any_known_value -> ()
  | `Update_with (new_compilation_status : Hydra_compilation_status.t) ->
    let now = Query.at query in
    let has_changed = ref (not had_bookmark_before) in
    Map.iteri new_compilation_status ~f:(fun ~key ~data:{ finished; pending } ->
      let pending =
        let known_pending =
          match Hashtbl.find t.compilation_status key with
          | None -> []
          | Some { Compilation_status.finished = _; pending } -> pending
        in
        List.map pending ~f:(fun { first_12_of_rev; rev_author_or_error } ->
          let the_one (t : Compilation_status.Pending_rev.t) =
            Node_hash.First_12.equal first_12_of_rev t.first_12_of_rev
          in
          match List.find known_pending ~f:the_one with
          | Some x -> x
          | None   -> { first_12_of_rev; rev_author_or_error; pending_since = now })
      in
      let new_compilation_status = { Compilation_status. finished; pending } in
      Hashtbl.update t.compilation_status key ~f:(fun previous_compilation_status ->
        (has_changed :=
           !has_changed ||
           (match previous_compilation_status with
            | None -> true
            | Some previous_compilation_status ->
              not (Compilation_status.equal
                     previous_compilation_status
                     new_compilation_status)));
        new_compilation_status));
    (* If the [compilation_status] has changed as a result of this new compilation status,
       we need to make sure to invalidate the feature caches.  If the feature had no
       bookmark previously, the invalidation has already happened in
       [set_has_bookmark_internal _]. *)
    if had_bookmark_before && !has_changed then invalidate_dependents t
;;

let set_lines_required_to_separate_ddiff_hunks t query
      lines_required_to_separate_ddiff_hunks =
  if not (Feature_path.is_root t.feature_path)
  then
    Or_error.error_s
      [%sexp
        "[lines-required-to-separate-ddiff-hunks] can be set for root features only"
      , (t.feature_path : Feature_path.t)
      ]
  else
  if lines_required_to_separate_ddiff_hunks < 0
  then
    Or_error.error_s
      [%sexp
        "[lines-required-to-separate-ddiff-hunks] shall be set to a non negative value"
      , (lines_required_to_separate_ddiff_hunks : int)
      ]
  else (
    let should_set =
      match t.lines_required_to_separate_ddiff_hunks with
      | None -> true
      | Some x -> x <> lines_required_to_separate_ddiff_hunks
    in
    (if should_set
     then (
       record t query
         (`Set_lines_required_to_separate_ddiff_hunks
            lines_required_to_separate_ddiff_hunks);
       t.lines_required_to_separate_ddiff_hunks <-
         Some lines_required_to_separate_ddiff_hunks));
    Ok ())
;;

let set_seconder t query user =
  match t.seconder, user with
  | Some user, Some _ -> error "already seconded by" user [%sexp_of: User_name.t]
  | None , None -> Ok ()
  | Some _ , None
  | None , Some _ ->
    record t query (`Set_seconder user);
    t.seconder <- user;
    Ok ()
;;

let set_description t query description =
  record t query (`Set_description description);
  t.description <- Feature_description.of_string description;
;;

let force_hydra_retry t =
  t.bounded_hydra_retry <- Bounded_hydra_retry.force_next_time t.bounded_hydra_retry;
  refresh_next_bookmark_update t;
;;

let set_is_permanent t query bool =
  record t query (`Set_is_permanent bool);
  t.is_permanent <- bool;
;;

let set_review_is_enabled t query bool =
  record t query (`Set_review_is_enabled bool);
  t.review_is_enabled <- bool;
;;

let set_reviewing t query users =
  record t query (`Set_reviewing users);
  t.reviewing <- users;
;;

let set_crs_are_enabled t query bool =
  record t query (`Set_crs_are_enabled bool);
  t.crs_are_enabled <- bool;
;;

let set_crs_shown_in_todo_only_for_users_reviewing t query bool =
  record t query (`Set_crs_shown_in_todo_only_for_users_reviewing bool);
  t.crs_shown_in_todo_only_for_users_reviewing <- bool;
;;

let set_xcrs_shown_in_todo_only_for_users_reviewing t query bool =
  record t query (`Set_xcrs_shown_in_todo_only_for_users_reviewing bool);
  t.xcrs_shown_in_todo_only_for_users_reviewing <- bool;
;;

let set_owners t query owners =
  if List.is_empty owners
  then error_string "must have at least one owner"
  else (
    record t query (`Set_owners owners);
    t.owners <- owners;
    Ok ())
;;

let set_whole_feature_followers t query users =
  record t query (`Set_whole_feature_followers users);
  t.whole_feature_followers <- users;
;;

let set_whole_feature_reviewers t query users =
  record t query (`Set_whole_feature_reviewers users);
  t.whole_feature_reviewers <- users;
;;

let set_release_process t query how =
  record t query (`Set_release_process how);
  t.release_process <- how;
;;

let set_who_can_release_into_me t query who =
  record t query (`Set_who_can_release_into_me who);
  t.who_can_release_into_me <- who;
;;

let set_send_email_to t query whom =
  record t query (`Set_send_email_to whom);
  t.send_email_to <- whom;
;;

let set_send_email_upon t query upon =
  record t query (`Set_send_email_upon upon);
  t.send_email_upon <- upon;
;;

let set_send_release_email_to t query whom =
  record t query (`Set_send_release_email_to whom);
  t.send_email_to <- whom;
;;

let set_should_send_release_email t query bool =
  record t query (`Set_should_send_release_email bool);
  t.send_email_upon <- Set.add t.send_email_upon Release;
;;

let set_inheritable_crs_shown_in_todo_only_for_users_reviewing t query option =
  record t query (`Set_inheritable_crs_shown_in_todo_only_for_users_reviewing option);
  Feature_inheritable_attributes.set_crs_shown_in_todo_only_for_users_reviewing
    t.inheritable_attributes option;
;;

let set_inheritable_xcrs_shown_in_todo_only_for_users_reviewing t query option =
  record t query (`Set_inheritable_xcrs_shown_in_todo_only_for_users_reviewing option);
  Feature_inheritable_attributes.set_xcrs_shown_in_todo_only_for_users_reviewing
    t.inheritable_attributes option;
;;

let set_inheritable_owners t query owners =
  record t query (`Set_inheritable_owners owners);
  Feature_inheritable_attributes.set_owners t.inheritable_attributes owners;
;;

let set_inheritable_properties t query properties =
  record t query (`Set_inheritable_properties properties);
  Feature_inheritable_attributes.set_properties t.inheritable_attributes properties;
;;

let set_inheritable_release_process t query how =
  record t query (`Set_inheritable_release_process how);
  Feature_inheritable_attributes.set_release_process t.inheritable_attributes how
;;

let set_inheritable_who_can_release_into_me t query who =
  record t query (`Set_inheritable_who_can_release_into_me who);
  Feature_inheritable_attributes.set_who_can_release_into_me
    t.inheritable_attributes who;
;;

let set_inheritable_send_email_to t query whom =
  record t query (`Set_inheritable_send_email_to whom);
  Feature_inheritable_attributes.set_send_email_to
    t.inheritable_attributes whom;
;;

let set_inheritable_send_email_upon t query upon =
  record t query (`Set_inheritable_send_email_upon upon);
  Feature_inheritable_attributes.set_send_email_upon
    t.inheritable_attributes upon;
;;

let set_inheritable_whole_feature_followers t query followers =
  record t query (`Set_inheritable_whole_feature_followers followers);
  Feature_inheritable_attributes.set_whole_feature_followers
    t.inheritable_attributes followers
;;

let set_inheritable_whole_feature_reviewers t query reviewers =
  record t query (`Set_inheritable_whole_feature_reviewers reviewers);
  Feature_inheritable_attributes.set_whole_feature_reviewers
    t.inheritable_attributes reviewers
;;

let apply_bookmark_update t bookmark_update =
  let latest_bookmark_update =
    match bookmark_update with
    | Error _ as e -> e
    | Ok { Bookmark_update.
           base_facts
         ; tip
         ; tip_facts
         ; base_is_ancestor_of_tip
         } ->
      t.bounded_hydra_retry <- Bounded_hydra_retry.empty;
      Option.iter base_facts ~f:(fun base_facts ->
        t.base_facts <- Known base_facts;
        (* pick up the tag that the worker computed *)
        t.base <- Rev_facts.rev base_facts;
      );
      t.tip <- tip;
      Option.iter tip_facts ~f:(fun tip_facts -> t.tip_facts <- Known tip_facts);
      Option.iter base_is_ancestor_of_tip ~f:(fun base_is_ancestor_of_tip ->
        t.base_is_ancestor_of_tip <- Known base_is_ancestor_of_tip);
      Ok ()
  in
  t.latest_bookmark_update <- latest_bookmark_update;
  (* Due to the way hydra sends synchronize-state rpcs, we'd receive one perhaps between
     5s and 10s after this bookmark-update finishes.  So let's instead remove this
     gratuitous delay by considering that hydra is done if its tip matches the tip we
     received.  Now, this is racy because we could consider that hydra is done, and
     receive immediately a synchronize-state that makes the feature pending again.  So we
     set [override_n_pending_status_if_needed] to prevent this.  If hydra were more
     reactive (i.e. sent a synchronize-state right after iron-hydra jobs finish), we could
     probably get rid of this. *)
  (match t.hydra_master_state with
   | None -> ()
   | Some { tip = last_known_hydra_tip
          ; status = _
          ; override_n_pending_status_if_needed = _
          } ->
     if Rev.has_prefix t.tip last_known_hydra_tip
     then (
       t.hydra_master_state <-
         Some { status = `Done
              ; tip = last_known_hydra_tip
              ; override_n_pending_status_if_needed = 2
              }));
  refresh_next_bookmark_update t;
;;

let update_cr_soons t cr_soons =
  let should_set =
    match t.cr_soons with
    | Pending_since _ | Known (Error _) -> true
    | Known (Ok old_cr_soons) ->
      match cr_soons with
      | Ok cr_soons -> not (Cr_soons.In_feature.equal cr_soons old_cr_soons)
      | Error _ ->
        (* We leave around the valid cr_soons that we have, to avoid pointless
           flickering. *)
        false
  in
  if should_set then set_cr_soons t (Known cr_soons);
;;

let rename_cr_soons t ~to_ =
  match t.cr_soons with
  | Pending_since _ | Known (Error _) -> ()
  | Known (Ok cr_soons) ->
    set_cr_soons t (Known (Ok (Cr_soons.In_feature.rename_non_root cr_soons ~to_)));
;;

let rename_non_root t query ~to_ =
  set_feature_path t query to_;
  rename_cr_soons t ~to_;
;;

let update_bookmark t query =
  let { Iron_protocol.Update_bookmark.Action. info; _ } =
    Query.action query
  in
  let bookmark_update, errors =
    match info with
    | Error _ as e -> e, []
    | Ok { base_facts
         ; tip_facts
         ; base_allow_review_for
         ; base_is_ancestor_of_tip
         ; diff_from_base_to_tip
         ; diff4s
         ; cr_soons
         ; _
         } ->
      set_diffs t (Known diff_from_base_to_tip) (Known diff4s);
      (match base_allow_review_for with
       | Error _ -> ()
       | Ok allow_review_for -> set_allow_review_for t allow_review_for);
      update_cr_soons t cr_soons;
      let errors = ref [] in
      let error e = errors := e :: !errors in
      let base = t.base in
      let tip = Rev_facts.rev tip_facts in
      let bookmark_update =
        { Bookmark_update.
          base_facts =
            (match Rev_facts.check base_facts base with
             | Error e -> error e; None
             | Ok (_ : bool) -> Some base_facts)
        ; tip
        ; tip_facts =
            (match Rev_facts.check tip_facts tip with
             | Error e -> error e; None
             | Ok (_ : bool) -> Some tip_facts)
        ; base_is_ancestor_of_tip =
            (match Rev_facts.Is_ancestor.check base_is_ancestor_of_tip
                     ~ancestor:base ~descendant:tip
             with
             | Error e -> error e; None
             | Ok (_ : bool) -> Some base_is_ancestor_of_tip)
        }
      in
      Ok bookmark_update, !errors
  in
  apply_bookmark_update t bookmark_update;
  record t query (`Update_bookmark bookmark_update);
  match errors with
  | [] -> Ok ()
  | _ -> Error (Error.of_list errors)
;;

let synchronize_with_hydra t ~hydra_tip ~hydra_status =
  let hydra_status, override_n_pending_status_if_needed =
    match t.hydra_master_state, hydra_status with
    | Some { tip; override_n_pending_status_if_needed = n; status = `Done },
      `Pending_or_working_on_it
      when Node_hash.First_12.equal tip hydra_tip && n > 0
      ->
      `Done, n - 1
    | _ ->
      hydra_status, 0
  in
  let hydra_master_state : Hydra_master_state.t =
    { status = hydra_status
    ; tip = hydra_tip
    ; override_n_pending_status_if_needed
    }
  in
  t.hydra_master_state <- Some hydra_master_state;
  let result =
    match would_ask_for_a_bookmark_update t with
    | None ->
      `Do_not_retry
    | Some bounded_hydra_retry ->
      t.bounded_hydra_retry <- bounded_hydra_retry;
      (* We anticipate the next synchronize-state rpc, so that even after asking for the
         last retry, [next_bookmark_update] will be [Update_expected_since]. *)
      t.hydra_master_state <-
        Some { hydra_master_state
               with status = `Pending_or_working_on_it
                  ; override_n_pending_status_if_needed = 0 };
      `Retry
  in
  refresh_next_bookmark_update t;
  result
;;

let remove_properties t query props =
  let unknown_props =
    Set.filter props ~f:(fun prop -> not (Hashtbl.mem t.properties prop))
  in
  if Set.is_empty unknown_props
  then (
    record t query (`Remove_properties props);
    Set.iter props ~f:(fun prop -> Hashtbl.remove t.properties prop);
    Ok ())
  else
    error "unknown properties" unknown_props [%sexp_of: Property.Set.t]
;;

let remove_inheritable_properties t query props =
  let properties = (Feature_inheritable_attributes.properties t.inheritable_attributes) in
  let unknown_props =
    Set.filter props ~f:(fun prop -> not (Map.mem properties prop))
  in
  if Set.is_empty unknown_props
  then (
    record t query (`Remove_inheritable_properties props);
    Feature_inheritable_attributes.remove_properties t.inheritable_attributes props;
    Ok ())
  else
    error "unknown properties" unknown_props [%sexp_of: Property.Set.t]
;;

let set_properties t query properties =
  record t query (`Set_properties properties);
  Map.iteri properties ~f:(fun ~key ~data -> Hashtbl.set t.properties ~key ~data);
;;

let lock t ~query ~for_ ~lock_name ~reason ~is_permanent =
  match
    match (lock_name : Lock_name.t) with
    | Create_child
    | Rebase
    | Release
    | Release_into
    | Rename
      -> Ok ()
    | Second ->
      if Option.is_some t.seconder
      then error_s [%sexp "Feature is already seconded."]
      else Ok ()
  with
  | Error _ as error -> error
  | Ok () ->
    record t query (`Lock { for_ ; lock_name ; reason; is_permanent });
    Feature_locks.lock t.locks ~query ~for_ ~lock_name ~reason ~is_permanent;
    Ok ()
;;

let unlock t ~query ~for_ ~lock_name ~even_if_permanent =
  Or_error.map (Feature_locks.unlock t.locks ~for_ ~lock_name ~even_if_permanent)
    ~f:(fun () -> record t query (`Unlock { for_ ; lock_name; even_if_permanent }))
;;

let apply_change_internal t query (action : Action.t) =
  match action with
  | `Ask_hydra_to_retry _ -> Ok ()
  | `Set_force_hydra_retry -> Ok ()
  | `Lock { Lock. for_; lock_name; reason; is_permanent } ->
    lock t ~query ~for_ ~reason ~lock_name ~is_permanent
  | `Remove_properties props -> remove_properties t query props
  | `Remove_inheritable_properties props -> remove_inheritable_properties t query props
  | `Set_base rev -> set_base t query rev; Ok ()
  | `Set_feature_path feature_path -> set_feature_path t query feature_path; Ok ()
  | `Set_description description -> set_description t query description; Ok ()
  | `Set_whole_feature_followers users ->
    set_whole_feature_followers t query users; Ok ()
  | `Set_whole_feature_reviewers users ->
    set_whole_feature_reviewers t query users; Ok ()
  | `Set_has_bookmark bool -> set_has_bookmark_internal t query bool; Ok ()
  | `Set_is_permanent bool -> set_is_permanent t query bool; Ok ()
  | `Set_owners users -> set_owners t query users
  | `Set_properties alist -> set_properties t query alist; Ok ()
  | `Set_review_is_enabled bool -> set_review_is_enabled t query bool; Ok ()
  | `Set_crs_are_enabled bool -> set_crs_are_enabled t query bool; Ok ()
  | `Set_crs_shown_in_todo_only_for_users_reviewing bool ->
    set_crs_shown_in_todo_only_for_users_reviewing t query bool; Ok ()
  | `Set_xcrs_shown_in_todo_only_for_users_reviewing bool ->
    set_xcrs_shown_in_todo_only_for_users_reviewing t query bool; Ok ()
  | `Set_seconder user_option -> set_seconder t query user_option
  | `Set_reviewing reviewing -> set_reviewing t query reviewing; Ok ()
  | `Unlock { Unlock. for_ ; lock_name; even_if_permanent } ->
    unlock t ~query ~for_ ~lock_name ~even_if_permanent
  | `Update_bookmark bookmark_update -> apply_bookmark_update t bookmark_update; Ok ()
  | `Set_inheritable_properties properties ->
    set_inheritable_properties t query properties; Ok ()
  | `Set_inheritable_owners owners -> set_inheritable_owners t query owners; Ok ()
  | `Set_inheritable_release_process how ->
    set_inheritable_release_process t query how; Ok ()
  | `Set_inheritable_whole_feature_followers followers ->
    set_inheritable_whole_feature_followers t query followers; Ok ()
  | `Set_inheritable_whole_feature_reviewers reviewers ->
    set_inheritable_whole_feature_reviewers t query reviewers; Ok ()
  | `Set_inheritable_crs_shown_in_todo_only_for_users_reviewing option ->
    set_inheritable_crs_shown_in_todo_only_for_users_reviewing t query option; Ok ()
  | `Set_inheritable_xcrs_shown_in_todo_only_for_users_reviewing option ->
    set_inheritable_xcrs_shown_in_todo_only_for_users_reviewing t query option; Ok ()
  | `Set_inheritable_who_can_release_into_me who
    -> set_inheritable_who_can_release_into_me t query who; Ok ()
  | `Set_inheritable_send_email_to whom -> set_inheritable_send_email_to t query whom;
    Ok ()
  | `Set_inheritable_send_email_upon upon -> set_inheritable_send_email_upon t query upon;
    Ok ()
  | `Set_lines_required_to_separate_ddiff_hunks lines_required_to_separate_ddiff_hunks ->
    set_lines_required_to_separate_ddiff_hunks t query
      lines_required_to_separate_ddiff_hunks
  | `Set_release_process how -> set_release_process t query how; Ok ()
  | `Set_who_can_release_into_me who -> set_who_can_release_into_me t query who; Ok ()
  | `Set_send_release_email_to whom -> set_send_release_email_to t query whom; Ok ()
  | `Set_should_send_release_email bool ->
    set_should_send_release_email t query bool; Ok ()
  | `Set_send_email_to whom -> set_send_email_to t query whom; Ok ()
  | `Set_send_email_upon upon -> set_send_email_upon t query upon; Ok ()
;;

let apply_query_internal t query =
  ok_exn (apply_change_internal t query (Query.action query))
;;

module Change_feature = Iron_protocol.Change_feature

module Add_remove
    (T : sig
       type t [@@deriving sexp_of]
       include Comparable.S_plain with type t := t
     end) : sig

  val add_list
    : T.t list
    -> T.t list
    -> (T.t list -> 'a)
    -> desc:string
    -> 'a Or_error.t

  val remove_list
    : T.t list
    -> T.t list
    -> (T.t list -> 'a)
    -> desc:string
    -> 'a Or_error.t

  val add
    : T.Set.t
    -> T.Set.t
    -> (T.Set.t -> 'a)
    -> desc:string
    -> 'a Or_error.t

  val remove
    : T.Set.t
    -> T.Set.t
    -> (T.Set.t -> 'a)
    -> desc:string
    -> 'a Or_error.t

end = struct

  let mem t ts = List.mem ts t ~equal:T.equal

  let intersect t1 t2 = List.filter t1 ~f:(fun t -> mem t t2)

  let diff t1 t2 = List.filter t1 ~f:(fun t -> not (mem t t2))

  let add_list current ts make ~desc =
    let already_present = intersect current ts in
    if List.is_empty already_present
    then Ok (make (current @ ts))
    else error desc already_present [%sexp_of: T.t list]
  ;;

  let remove_list current ts make ~desc =
    let not_present = diff ts current in
    if List.is_empty not_present
    then Ok (make (diff current ts))
    else error desc not_present [%sexp_of: T.t list]
  ;;

  let add current ts make ~desc =
    let already_present = Set.inter current ts in
    if Set.is_empty already_present
    then Ok (make (Set.union current ts))
    else error desc already_present [%sexp_of: T.Set.t]
  ;;

  let remove current ts make ~desc =
    let not_present = Set.diff ts current in
    if Set.is_empty not_present
    then Ok (make (Set.diff current ts))
    else error desc not_present [%sexp_of: T.Set.t]
  ;;
end

module Email_addresses  = Add_remove (Email_address)
module Property_set     = Add_remove (Property)
module Send_email_upons = Add_remove (Send_email_upon)
module User_names       = Add_remove (User_name)

let change t query (updates : Change_feature.Update.t list) =
  let set_owners x = `Set_owners x in
  let set_send_email_to x = `Set_send_email_to x in
  let set_send_email_upon x = `Set_send_email_upon x in
  let set_whole_feature_followers x = `Set_whole_feature_followers x in
  let set_whole_feature_reviewers x = `Set_whole_feature_reviewers x in
  let set_reviewing x = `Set_reviewing (`Only x) in
  let set_reviewing_all_but x =
    `Set_reviewing (if Set.is_empty x then `All else `All_but x)
  in
  let set_inheritable_owners x = `Set_inheritable_owners x in
  let set_inheritable_send_email_to x = `Set_inheritable_send_email_to x in
  let set_inheritable_send_email_upon x = `Set_inheritable_send_email_upon x in
  let set_inheritable_whole_feature_followers x =
    `Set_inheritable_whole_feature_followers x
  in
  let set_inheritable_whole_feature_reviewers x =
    `Set_inheritable_whole_feature_reviewers x
  in
  let action_for (update : Change_feature.Update.t) =
    match update with
    | `Add_owners users ->
      User_names.add_list (owners t) users set_owners ~desc:"already an owner"
    | `Remove_owners users ->
      User_names.remove_list (owners t) (Set.to_list users)
        set_owners ~desc:"not currently an owner"
    | `Add_whole_feature_followers users ->
      User_names.add t.whole_feature_followers users set_whole_feature_followers
        ~desc:"already a whole-feature follower"
    | `Remove_whole_feature_followers users ->
      User_names.remove t.whole_feature_followers users set_whole_feature_followers
        ~desc:"not currently a whole-feature follower"
    | `Add_whole_feature_reviewers users ->
      User_names.add t.whole_feature_reviewers users set_whole_feature_reviewers
        ~desc:"already a whole-feature reviewer"
    | `Remove_whole_feature_reviewers users ->
      User_names.remove t.whole_feature_reviewers users set_whole_feature_reviewers
        ~desc:"not currently a whole-feature reviewer"
    | `Add_reviewing users ->
      (match t.reviewing with
       | `All -> error_string "everyone is already reviewing"
       | `Whole_feature_reviewers ->
         User_names.add t.whole_feature_reviewers users set_reviewing
           ~desc:"already reviewing"
       | `All_but not_reviewing ->
         User_names.remove not_reviewing users set_reviewing_all_but
           ~desc:"already reviewing"
       | `Only reviewing ->
         User_names.add reviewing users set_reviewing ~desc:"already reviewing")
    | `Remove_reviewing users ->
      (match t.reviewing with
       | `All -> Ok (set_reviewing_all_but users)
       | `Whole_feature_reviewers ->
         User_names.remove t.whole_feature_reviewers users set_reviewing
           ~desc:"not reviewing"
       | `All_but not_reviewing ->
         User_names.add not_reviewing users set_reviewing_all_but ~desc:"not reviewing"
       | `Only reviewing ->
         User_names.remove reviewing users set_reviewing ~desc:"not reviewing")
    | `Add_send_email_to users ->
      Email_addresses.add (send_email_to t) users set_send_email_to
        ~desc:"already sending email to"
    | `Remove_send_email_to users ->
      Email_addresses.remove (send_email_to t) users set_send_email_to
        ~desc:"weren't sending email to"
    | `Add_send_email_upon upons ->
      Send_email_upons.add (send_email_upon t) upons set_send_email_upon
        ~desc:"already sending email upon"
    | `Remove_send_email_upon upons ->
      Send_email_upons.remove (send_email_upon t) upons set_send_email_upon
        ~desc:"weren't sending email upon"
    | `Add_inheritable_owners owners ->
      User_names.add_list
        (Feature_inheritable_attributes.owners t.inheritable_attributes)
        owners set_inheritable_owners ~desc:"is already inheritable owner"
    | `Remove_inheritable_owners owners ->
      User_names.remove_list
        (Feature_inheritable_attributes.owners t.inheritable_attributes)
        (Set.to_list owners) set_inheritable_owners
        ~desc:"owner is not inheritable"
    | `Add_inheritable_whole_feature_followers whole_feature_followers ->
      User_names.add
        (Feature_inheritable_attributes.whole_feature_followers
           t.inheritable_attributes)
        whole_feature_followers set_inheritable_whole_feature_followers
        ~desc:"is already inheritable whole-feature follower"
    | `Remove_inheritable_whole_feature_followers whole_feature_followers ->
      User_names.remove
        (Feature_inheritable_attributes.whole_feature_followers
           t.inheritable_attributes)
        whole_feature_followers set_inheritable_whole_feature_followers
        ~desc:"is not inheritable whole-feature follower"
    | `Add_inheritable_whole_feature_reviewers whole_feature_reviewers ->
      User_names.add
        (Feature_inheritable_attributes.whole_feature_reviewers
           t.inheritable_attributes)
        whole_feature_reviewers set_inheritable_whole_feature_reviewers
        ~desc:"is already inheritable whole-feature reviewer"
    | `Remove_inheritable_whole_feature_reviewers whole_feature_reviewers ->
      User_names.remove
        (Feature_inheritable_attributes.whole_feature_reviewers
           t.inheritable_attributes)
        whole_feature_reviewers set_inheritable_whole_feature_reviewers
        ~desc:"is not inheritable whole-feature reviewer"
    | `Add_inheritable_send_email_to send_email_to ->
      Email_addresses.add
        (Feature_inheritable_attributes.send_email_to t.inheritable_attributes)
        send_email_to set_inheritable_send_email_to
        ~desc:"send_email_to is already inheritable"
    | `Remove_inheritable_send_email_to send_email_to ->
      Email_addresses.remove
        (Feature_inheritable_attributes.send_email_to t.inheritable_attributes)
        send_email_to set_inheritable_send_email_to
        ~desc:"send_email_to is not inheritable"
    | `Add_inheritable_send_email_upon send_email_upon ->
      Send_email_upons.add
        (Feature_inheritable_attributes.send_email_upon t.inheritable_attributes)
        send_email_upon set_inheritable_send_email_upon
        ~desc:"send_email_upon is already inheritable"
    | `Remove_inheritable_send_email_upon send_email_upon ->
      Send_email_upons.remove
        (Feature_inheritable_attributes.send_email_upon t.inheritable_attributes)
        send_email_upon set_inheritable_send_email_upon
        ~desc:"send_email_upon is not inheritable"
    | ( `Remove_properties _
      | `Remove_inheritable_properties _
      | `Set_base _
      | `Set_crs_are_enabled _
      | `Set_crs_shown_in_todo_only_for_users_reviewing _
      | `Set_xcrs_shown_in_todo_only_for_users_reviewing _
      | `Set_description _
      | `Set_is_permanent _
      | `Set_inheritable_crs_shown_in_todo_only_for_users_reviewing _
      | `Set_inheritable_xcrs_shown_in_todo_only_for_users_reviewing _
      | `Set_inheritable_owners _
      | `Set_inheritable_properties _
      | `Set_inheritable_release_process _
      | `Set_inheritable_who_can_release_into_me _
      | `Set_inheritable_send_email_upon _
      | `Set_inheritable_send_email_to _
      | `Set_inheritable_whole_feature_followers _
      | `Set_inheritable_whole_feature_reviewers _
      | `Set_lines_required_to_separate_ddiff_hunks _
      | `Set_owners _
      | `Set_properties _
      | `Set_release_process _
      | `Set_review_is_enabled _
      | `Set_reviewing _
      | `Set_send_email_to _
      | `Set_send_email_upon _
      | `Set_who_can_release_into_me _
      | `Set_whole_feature_followers _
      | `Set_whole_feature_reviewers _
      ) as update -> Ok (update :> Action.t)
  in
  List.map updates ~f:(fun update ->
    update, Result.bind (action_for update) ~f:(apply_change_internal t query))
;;

let deserializer ~dynamic_upgrade_state =
  Deserializer.with_serializer (fun serializer ->
    let open Deserializer.Let_syntax in
    let%map_open () = return ()
    and cr_soons =
      one (module Persist.Cr_soons_or_pending) ~in_file:cr_soons_file
    and creation =
      one (module Persist.Creation) ~in_file:creation_file
    and queries  =
      sequence_of (module Persist.Action_query) ~in_file:queries_file
    and diff_from_base_to_tip =
      one (module Persist.Diff_from_base_to_tip) ~in_file:diff_from_base_to_tip_file
    and diff4s = one (module Persist.Diff4s_file) ~in_file:diff4s_file
    and included_features =
      sequence_of (module Persist.Included_feature) ~in_file:included_features_file
    and allow_review_for =
      one (module Persist.Allow_review_for) ~in_file:allow_review_for_file
        ~default:Allow_review_for.all
    and latest_release =
      one_opt (module Persist.Latest_release) ~in_file:latest_release_file
    in
    let t = create_internal creation ~dynamic_upgrade_state ~serializer:None in
    List.iter queries ~f:(fun query -> apply_query_internal t query);
    set_diffs_internal t diff_from_base_to_tip diff4s;
    set_allow_review_for_internal t allow_review_for;
    set_cr_soons_internal t cr_soons;
    Option.iter latest_release ~f:(set_latest_release_internal t);
    t.included_features <- List.rev included_features;
    t.serializer <- Some serializer;
    Serializer.add_cache_invalidator serializer t.cache_invalidator;
    assert (Option.is_none t.hydra_master_state);
    (* See the comment on [needs_bookmark_update] for why we have to refresh here. *)
    refresh_next_bookmark_update t;
    t)
;;
