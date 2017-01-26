(** A set of CR-soons, where the assignee is an unresolved name. *)

open! Core
open! Import

module Cr_soon_in_feature : sig

  type t [@@deriving sexp_of]

  val active_in    : t -> Feature_path.t option
  val cr_comment   : t -> Cr_comment.t
  val feature_path : t -> Feature_path.t
  val path         : t -> Relpath.t
  val start_line   : t -> int

  module For_sorted_output : sig
    type nonrec t = t [@@deriving compare]
  end
end

type t [@@deriving sexp_of]

include Equal.S     with type t := t
include Invariant.S with type t := t

val empty : t

val is_empty : t -> bool

val create : Cr_comment.Cr_soon.t list -> Feature_path.t -> t

val choose_feature_path : t -> Feature_path.t option

(** [rename_feature t ~from to_] replaces in [t] all occurences of [from] with [to_]. *)
val rename_feature : t -> from:Feature_path.t -> to_:Feature_path.t -> t

val to_list : t -> Cr_soon_in_feature.t list

val iter : t -> f:(Cr_soon_in_feature.t -> num_occurrences:int -> unit) -> unit

val diff : t -> t -> t

val union  : t -> t -> t
val unions : t list -> t

val partition_by_assignee : t -> User_name_by_alternate_name.t -> t User_name.Map.t

val filter : t -> f:(Cr_soon_in_feature.t -> bool) -> t

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
  module Cr_soon_in_feature : sig
    module V1 : Stable_without_comparator with type t = Cr_soon_in_feature.t
  end
end
