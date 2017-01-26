open! Core
open! Import

(** This type is used to manipulate a diff4 that is shown to a user *)
type t [@@deriving sexp_of]

val review   : Diff4_in_session.And_review_kind.t -> t
val catch_up : Diff4_to_catch_up.t -> t

val diff4       : t -> Diff4.t
val id          : t -> Diff4_in_session.Id.t
val is_reviewed : t -> bool
val path_in_repo_at_f2 : t -> Path_in_repo.t

val compare_by_path_in_repo_at_f2_for_review : t -> t -> int

val num_lines : t -> Reviewer.t -> int

module Kind : sig
  type t =
    | Catch_up of Catch_up_kind.t
    | Review   of Review_kind.t
  [@@deriving compare, sexp_of]

  val compare_to_sort_by_decreasing_priority : t -> t -> int

  val to_string_hum : t -> string
end

val kind : t -> Kind.t

val by_kind_of_decreasing_priority : t list -> (Kind.t * t list) list
