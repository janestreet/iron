(** Everything related to a user's work on a single feature.

    - the set of CRs/XCRs assigned to the user at the tip of the feature
    - review goal: the diff the user is obliged to know
    - known: the diff the user knows
    - current session: an optional [Review_session.t]
    - allow_review_for: who is allowed to modify this review manager

    [get_session_exn] creates a session from known to goal if there isn't already
    one.  Until a diff4 is reviewed in the session, the session is "provisional", and may
    be thrown away by a subsequent call to [get_session_exn].  [num_lines_*]
    functions ignore provisional sessions.  We don't aggressively delete provisional
    sessions (e.g. in [update_review_goal]) to avoid annoying the user by deleting a
    session that they are actually looking at and subsequently requesting a request to
    mark a diff4 as reviewed.
*)

open! Core
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

module Register_catch_up : sig
  type t =
    { f : 'a .
            'a Query.t
        -> Review_session.t
        -> Catch_up_session.Id_and_kind.t list
        -> unit Or_error.t
    }
end

val deserializer
  : (whole_feature_followers        : User_name.Set.t
     -> whole_feature_reviewers     : User_name.Set.t
     -> users_using_locked_sessions : User_name.Set.t
     -> review_goal                 : Review_goal.t Or_error.t
     -> indexed_diff4s              : Indexed_diff4s.t Or_error.t Or_pending.t
     -> register_catch_up           : Register_catch_up.t
     -> feature_cache_invalidator   : Cached.Invalidator.t
     -> dynamic_upgrade_state       : Dynamic_upgrade.State.t
     -> t
    ) Deserializer.t

val create
  :  User_name.t
  -> is_whole_feature_follower : bool
  -> is_whole_feature_reviewer : bool
  -> is_using_locked_sessions  : bool
  -> cr_comments               : Cr_comment.t list
  -> base_facts                : Rev_facts.t Or_pending.t
  -> register_catch_up         : Register_catch_up.t
  -> feature_cache_invalidator : Cached.Invalidator.t
  -> dynamic_upgrade_state     : Dynamic_upgrade.State.t
  -> Serializer.t
  -> t

(** Accessors. *)
val brain : t -> Brain.t
val user_name : t -> User_name. t
val is_whole_feature_follower : t -> bool
val is_whole_feature_reviewer : t -> bool

val reviewer : t -> Reviewer.t

val have_done_some_review : t -> bool
val have_session_in_progress : t -> bool
val reviewed_diff4s_output_in_current_session : t -> Diff2.t list

module Goal_subset : sig
  (** When the goal is satisfied by the review done by other users, we make the count drop
      to zero lines.  That way we stop showing features in users' todo and in the line
      count table of fe show if the users are not required to make the feature fully
      reviewed.

      More specifically, in the [Entire_goal] case, the line count shown is the size of
      what the user may choose to read, but when the review_analysis is provided, it is
      the size of the review goal of the reviewer, assuming that review goal is not
      already satisfied by other people's review and by the other wfrs review goal. This
      is different from the best possible criteria, namely "the size of the review goal if
      reading it moves the feature closer to being releasable", because someone else may
      need to read part of the feature but haven't yet, and if they did the current
      reviewer wouldn't need to read the feature anymore.

      In general, except when computing [review_analysis] itself or the
      reviewer is known to be needed (eg whole feature reviewer), the review_analysis
      should be provided. *)
  type t =
    | Entire_goal
    | Entire_goal_but_empty_if_satisfied_by of Review_analysis.t
end

module Review_authorization : sig
  (** A value of this type is a witness of the context under which it is allowed to
      perform some operations on a given review manager.

      This also allows breaking circular dependency between [Review_manager], [State] and
      [User_info] *)
  type t
  type review_manager

  val create
    :  review_manager
    -> allow_review_for : Allow_review_for.t
    -> are_acting_for_themselves_or_for_invalid_user :
         (for_:User_name.t -> by:User_name.t -> bool)
    -> current_feature_goal_subset : Goal_subset.t
    -> reason:[ `Not_supported | `This of string ]
    -> create_catch_up_for_me:bool
    -> _ Query.t
    -> t Or_error.t

  val unauthorized_for_user_with_only_follow_lines : t -> unit Or_error.t

end with type review_manager := t

(** [line_count_remaining_to_review] tries to be consistent with [fe review] when the
    review goal is an error, since this is used in todo. *)
val line_count_remaining_to_review
  :  t
  -> Goal_subset.t
  -> Line_count.Review.t To_goal_via_session.t

val line_count_cached_in_feature
  :  t
  -> Goal_subset.t
  -> Line_count.Cached_in_feature.t

val dump_review_lines
  :  t
  -> Goal_subset.t
  -> Sexp.t

(** [can_make_progress t = (num_lines_remaining t goal_subset).review > 0]. *)
val can_make_progress
  :  t
  -> Goal_subset.t
  -> bool

val crs      : t -> Cr_comment.t list Or_error.t
val num_crs  : t -> int Or_error.t
val num_xcrs : t -> int Or_error.t

val check_cr_clean       : t -> unit Or_error.t

val reviewed
  :  t
  -> _ Query.t
  -> Session_id.t
  -> Diff4_in_session.Id.t list
  -> Goal_subset.t
  -> Review_authorization.t
  -> even_if_some_files_are_already_reviewed:bool
  -> unit Or_error.t

val forget_from_brain_exn
  :  t
  -> Review_authorization.t
  -> what_to_forget:[ `All
                    | `Files of Path_in_repo.t list
                    ]
  -> unit

val forget_from_current_session_exn
  :  t
  -> Review_authorization.t
  -> _ Query.t
  -> Session_id.t
  -> what_to_forget:[ `All
                    | `Files of Path_in_repo.t list
                    ]
  -> unit

val get_session_exn
  :  t
  -> Goal_subset.t
  -> may_be_reviewed_by : Allow_review_for.Users.t
  -> lines_required_to_separate_ddiff_hunks: int
  -> which_session : Which_session.t
  -> [ `Review_session of Iron_protocol.Get_review_session.Review_session.t
     | `Up_to_date
     ]

val commit_current_session_exn
  :  t
  -> Review_authorization.t
  -> Session_id.t
  -> unit

val set_session_is_locked_exn
  :  t
  -> Review_authorization.t
  -> _ Query.t
  -> which_session: Which_session.t
  -> bool
  -> unit

val update_crs
  :  t
  -> Cr_comment.t list
  -> base_facts:Rev_facts.t Or_pending.t
  -> unit

(** [diff4s] is computed by hydra, and is an [Or_error.t] to reflect that the computation
    can fail on the hydra side. *)
val update_review_goal
  :  t
  -> review_goal               : Review_goal.t Or_error.t
  -> indexed_diff4s            : Indexed_diff4s.t Or_error.t
  -> is_whole_feature_follower : bool
  -> is_whole_feature_reviewer : bool
  -> unit

val set_is_using_locked_sessions : t -> bool -> unit

val mark_fully_reviewed
  :  t
  -> _ Query.t
  -> [ `Internal_mark__no_catch_up_allow_for_all
     | `Review_authorization of Review_authorization.t
     ]
  -> unit

val need_diff4s_starting_from : t -> Review_edge.Set.t

val check_session_id : t -> Session_id.t -> unit Or_error.t

val de_alias_brain
  :  t
  -> User_name_by_alternate_name.t
  -> [ `De_aliased
     | `Did_not_de_alias_due_to_review_session_in_progress
     | `Nothing_to_do
     ]

val release
  :  t
  -> _ Query.t
  -> unit
