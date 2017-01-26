open! Core
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

(** Accessors. *)
val diff2s : t -> Diff2s.t

val base_rev : t -> Rev.t
val tip_rev  : t -> Rev.t

val base_facts : t -> Rev_facts.t
val tip_facts  : t -> Rev_facts.t
val base_is_ancestor_of_tip : t -> Rev_facts.Is_ancestor.t

val create
  :  Diff2s.t
  -> base_facts              : Rev_facts.t
  -> tip_facts               : Rev_facts.t
  -> base_is_ancestor_of_tip : Rev_facts.Is_ancestor.t
  -> t Or_error.t

val review_edge : t -> Review_edge.t

val restrict_to_may_review_or_follow : t -> Reviewer.t -> t

val by_diff2 : t -> f:(Diff2.t -> 'a) -> 'a Diff2.Ignoring_rev.Table.t
