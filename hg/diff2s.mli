(** A set of [Diff2.t]s representing a diff from a base to a tip, a subset of such a diff,
    or, a reviewer's knowledge of the changes in a feature, possibly with different base
    and tip revisions for different elements in the set.

    For consistency, a [Diff2s.t] has the property that for all distinct [d1] and [d2] in
    the set:

    {[
      not (Relpath.equal d1.tip.path_in_repo d2.tip.path_in_repo)
    ]}

    I.e., there can be at most one fact about the "current" state of a path in the repo.
*)

open! Core
open! Import

type t = Diff2.t list [@@deriving compare, sexp_of]

include Invariant.S with type t := t

val restrict_to_may_review_or_follow : t -> Reviewer.t -> t

val num_lines_for_feature_size : t -> int

val may_reviewers  : t -> include_file_followers:bool -> User_name.Set.t

module Stable : sig
  module V2 : Stable_without_comparator with type t = t
end
