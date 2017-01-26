open! Core
open! Import

(** This module only contains the types and some accessors to manipulate the review
    analysis, to be used by various modules such as review_manager without introducing a
    cyclic dependency.  The whole logic to build values of that types needs to access the
    server state and thus lives in [state.ml] *)

module Attributed_files_analysis : sig
  type t =
    { completed_review                                       : User_name.Set.t
    ; completed_review_satisfies_non_wfr_obligations_in_base : bool
    ; completed_review_satisfies_non_wfr_obligations_in_tip  : bool
    }
  [@@deriving sexp_of]
end

module Diff2_analysis : sig
  type t =
    { actually_reviewed                    : Attributed_files_analysis.t
    ; assuming_expected_users_are_finished : Attributed_files_analysis.t
    }
  [@@deriving sexp_of]
end

module Whole_feature_reviewer_analysis : sig
  type t =
    { obligations_are_satisfied : bool
    }
  [@@deriving sexp_of]
end

type t =
  { diff2s : Diff2_analysis.t Diff2.Ignoring_rev.Map.t
  ; users_with_review_session_in_progress : User_name.Set.t
  ; whole_feature_reviewers : Whole_feature_reviewer_analysis.t User_name.Map.t
  }
[@@deriving fields, sexp_of]

include Invariant.S with type t := t
include Equal.S with type t := t

(** If the diff2 has been reviewed sufficiently (ie we don't need more reviewers),
    assuming some expected users have read this diff2.  This heuristic is used to trim
    down the list of non wfr that end up including this diff2 when counting the review
    lines in their todo if they are not required to read to reach releasability.

    Expected users are:
    -whole feature reviewers
    -users who have reviewed this diff2 in their brain but have pending uncommitted
    session.

    The latter set is just a way to avoid some flickering in users todo under k-of-n
    obligations in case the trimming applies and the reviewers start/stop/start a
    session. *)
val non_wfr_obligations_will_be_satisfied_once_expected_users_have_read
  :  t
  -> Diff2.t
  -> bool

val obligations_are_satisfied : t -> bool
