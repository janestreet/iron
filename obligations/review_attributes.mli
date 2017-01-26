(** The metadata for each file as specified by .fe.sexp files. *)

open! Core
open! Import

type t = private
  { build_projections                  : Build_projection_name.Set.t
  ; tags                               : Tag.Set.t
  ; fewer_than_min_reviewers           : bool
  ; followers                          : User_name.Set.t
  ; is_read_by_whole_feature_reviewers : bool
  ; more_than_max_reviewers            : bool
  ; owner                              : User_name.t
  ; review_obligation                  : Review_obligation.t
  ; scrutiny_level                     : Scrutiny_level.t
  ; scrutiny_name                      : Scrutiny_name.t
  }
[@@deriving compare, fields, sexp_of]

val create
  :  build_projections                  : Build_projection_name.Set.t
  -> tags                               : Tag.Set.t
  -> fewer_than_min_reviewers           : bool
  -> followers                          : User_name.Set.t
  -> is_read_by_whole_feature_reviewers : bool
  -> more_than_max_reviewers            : bool
  -> owner                              : User_name.t
  -> review_obligation                  : Review_obligation.t
  -> scrutiny_level                     : Scrutiny_level.t
  -> scrutiny_name                      : Scrutiny_name.t
  -> t

val with_review_obligation
  : t
  -> review_obligation: Review_obligation.t
  -> t

include Comparable.S_plain with type t := t
include Equal.S            with type t := t

val attribute_table : t -> Ascii_table.t

val hash : t -> int
val module_name : string

val for_testing : t

module Stable : sig
  module V2 : Stable_without_comparator with type t = t
end
