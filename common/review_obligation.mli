(** Specifies who must review changes to a file. *)

open! Core
open! Import

type t = private
  | All_of        of User_name.Set.t
  | At_least_wide of int * User_name.Set.t
  | And           of t list
  | Or_wide       of t list
[@@deriving compare, sexp_of]

include Equal.S     with type t := t
include Invariant.S with type t := t

module type Reviewed_by = sig
  type review_obligation
  type t [@@deriving sexp_of]
  val synthesize : review_obligation -> t
end with type review_obligation := t

(** We like to display [Review_obligation.t] using the [Reviewed_by] syntax from
    [.fe.sexp] files, which is defined later in the [Iron_obligations] library.  So, we
    abstract out the display over a [synthesize] function that converts from
    [Review_obligation.t] to [Reviewed_by.t]. *)
val to_string_hum : (module Reviewed_by) -> t -> string

val none          : t

val all_of        : User_name.Set.t -> t

val at_least_wide : Error_context.t -> int -> User_name.Set.t -> t

(** [and_] normalizes, e.g. combines user sets and sorts conjuncts, to reduce spurious
    displays of review-obligation changes that are just reorderings. *)
val and_          : t list -> t

val or_wide       : Error_context.t -> t list -> t

val may_reviewers : t -> User_name.Set.t

(** [has_a_may_reviewer t] iff [Set.length (may_reviewers t) > 0]. *)
val has_a_may_reviewer : t -> bool

(** A lower bound on the size of any satisfying set of users. *)
val num_reviewers_lower_bound : t -> int

val may_review : t -> User_name.t -> bool

val is_satisfied : t -> by:User_name.Set.t -> bool

val de_alias : t -> User_name_by_alternate_name.t -> t

module Stable : sig
  module V1 : sig
    include Stable_without_comparator with type t = t
    val hash : t -> int
  end
end
