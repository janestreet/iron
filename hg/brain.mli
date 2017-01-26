open! Core
open! Import

module Mark_kind : sig
  type t =
    | User
    | Internal__fully_reviewed
  [@@deriving compare, sexp_of]
end

module Marked_diff2 : sig
  type t =
    { diff2     : Diff2.t
    ; mark_kind : Mark_kind.t
    }
  [@@deriving compare, fields, sexp_of]
end

type t = Marked_diff2.t list
[@@deriving compare, sexp_of]

include Invariant.S with type t := t

val empty : t

val review_edges : t -> Review_edge.Set.t

val extend : t -> with_:Diff4s.t -> reviewer:Reviewer.t -> mark_kind:Mark_kind.t -> t

val what_would_be_extended
  :  t
  -> (Diff4.t -> [ `Extends of Marked_diff2.t | `New of Diff2.t ] Or_error.t) Lazy.t

(** [diff4s_needed_to_extend_brain ~reviewer ~brain ~goal ~could_use] computes the diff4s
    that are needed to extend [reviewer]'s [brain] to [goal], where one can use any diff4
    in [could_use] or any diff2 in [goal].  This relates to [extend] via the equation:

    {[
      Map.find (by_tip_path goal) path
      =
      Map.find
        (by_tip_path
           (extend brain
              ~with_:(diff4s_needed_to_extend_brain brain ~goal ~could_show)))
        path
    ]}

    which holds for all [path]s in the domain of [by_tip_path goal].  The idea is
    that after the reviewer is shown the diffs, his knowledge agrees with [goal]
    wherever [goal] is defined, but that the review may have additional knowledge
    that wasn't extended, presumably because it is not required to be known. *)
val diff4s_needed_to_extend_brain
  :  t
  -> reviewer  : Reviewer.t
  -> goal      : Diff2s.t
  -> could_use : Indexed_diff4s.t
  -> Diff4s.t

(** Check that [diff4s_needed_to_extend_brain] returns a session that takes the [brain]
    all the way to [goal]. *)
val check_diff4s_needed_to_extend_brain_exn
  :  t
  -> reviewer  : Reviewer.t
  -> goal      : Diff2s.t
  -> could_use : Indexed_diff4s.t
  -> unit

val de_alias : t -> User_name_by_alternate_name.t -> t

module Stable : sig
  module Model : T with type t = t
  module V3 : Stable_without_comparator with type t = Model.t
  module V2 : sig
    include Stable_without_comparator with type t = Diff2s.t
    val to_model : t -> Model.t
    val of_model : Model.t -> t
  end
end
