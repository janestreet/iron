(** A per-repo Iron obligations file, [.fe/obligation-repo.sexp]. *)

open! Core.Std
open! Import

module Declaration : sig
  type t [@@deriving sexp_of]

  val of_annotated_sexp : Sexp.Annotated.t -> t

  val scaffold_requires_global_tag_or_rev_hash : t -> bool
end

type t = private
  { build_projections    : Build_projection.t Build_projection_name.Map.t
  ; tags                 : Tag.Set.t
  ; users                : Unresolved_name.Set.t
  ; groups               : Groups.t
  ; obligations_global   : Obligations_global.t
  ; allow_review_for     : Allow_review_for.t
  }
[@@deriving sexp_of]

val union_of_users_defined  : t -> Unresolved_name.Set.t
val union_of_groups_defined : t -> Groups.t
val union_of_tags_defined   : t -> Tag.Set.t

val eval
  :  Obligations_global.t
  -> Declaration.t list
  -> obligations_repo : Path.t
  -> aliases : User_name_by_alternate_name.t
  -> t Or_error.t

module Stable : sig
  module V4 : Stable_without_comparator with type t = t
end
