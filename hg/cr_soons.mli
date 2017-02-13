(** All CR-soons in the feature forest, where assignees have been resolved to user
    names.
*)

open! Core
open! Import

(** All CR-soons in a feature, where assignees are unresolved names. *)
module In_feature : sig

  type t [@@deriving sexp_of]

  include Equal.     S with type t := t
  include Invariant. S with type t := t

  val create
    :  feature_path            : Feature_path.t
    -> base_facts              : Rev_facts.t
    -> base_cr_soons           : Cr_soon.t list
    -> tip_facts               : Rev_facts.t
    -> tip_cr_soons            : Cr_soon.t list
    -> base_is_ancestor_of_tip : Rev_facts.Is_ancestor.t
    -> t Or_error.t

  val rename_non_root : t -> to_:Feature_path.t -> t
end

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val create : unit -> t

val update_feature : t -> In_feature.t -> unit
val remove_feature : t -> Feature_path.t -> unit

val repartition : t -> alternate_names:User_name_by_alternate_name.t -> unit

(** [rename_non_root_feature t ~from to_] renames non-root feature [from] to non-root
    feature [to_], and replaces all occurences of [from] to [to_] in that feature's
    multiset.  It raises if [to_] exists, or if [from] or [to_] is a root feature or if
    they don't have the same root. *)
val rename_non_root_feature
  :  t
  -> from : Feature_path.t
  -> to_  : Feature_path.t
  -> unit

(** apply [f] to every user assigned an inactive CR-soon.  [f] can be applied to the
    same user multiple times, up to once for each feature tree. *)
val iter_inactive_assignees
  :  t
  -> f:(User_name.t -> unit)
  -> unit

val get_all
  :  t
  -> for_or_all     : User_name.Or_all.t
  -> include_active : bool
  -> Cr_soon_multiset.t

val get_for_feature_tree
  :  t
  -> root_feature   : Feature_name.t
  -> for_or_all     : User_name.Or_all.t
  -> include_active : bool
  -> Cr_soon_multiset.t

module Stable : sig
  module In_feature : sig
    module V1 : Stable_without_comparator with type t = In_feature.t
  end
end
