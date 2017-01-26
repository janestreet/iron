open! Core
open! Import

module Action : sig
  type t =
    { feature_path     : Feature_path.t
    ; rev_zero         : Rev.t option
    ; for_             : User_name.t
    ; ensure_reviewing : bool
    ; which_session    : Which_session.t

    ; lock_session : [ `If_applicable
                     | `No
                     ]
    }
  [@@deriving fields, sexp_of]
end

module Line_count_to_goal : sig
  type 'a t =
    { from_session_end                    : 'a
    ; from_brain_if_session_was_committed : 'a
    }
  [@@deriving fields, sexp_of]

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module Review_session : sig
  type t =
    { review_session_id                        : Session_id.t
    ; review_session_tip                       : Rev.t
    ; reviewer_in_session                      : Reviewer.t
    ; reviewer_in_feature                      : Reviewer.t
    ; diff4s_in_session                        : Diff4_in_session.And_review_kind.t array
    ; may_be_reviewed_by                       : Allow_review_for.Users.t
    ; line_count_to_finish_session             : Line_count.Review.t
    ; line_count_to_goal
      : Line_count.Review.t Or_error.t Or_pending.t Line_count_to_goal.t
    ; is_locked                                : bool
    ; lines_required_to_separate_ddiff_hunks   : int
    }
  [@@deriving sexp_of]
end

module Reaction : sig
  type t =
    { status            : [ `Up_to_date
                          | `Bookmark_update_is_pending
                          | `Review_session of Review_session.t
                          ]
    ; feature_tip       : Rev.t
    ; remote_rev_zero   : Rev.t
    ; remote_repo_path  : Remote_repo_path.t
    ; may_second        : bool
    }
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
