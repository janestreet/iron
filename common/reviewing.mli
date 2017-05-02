(** A symbolic representation of the set of reviewing users. *)

open! Core
open! Import

type t =
  [ `All
  | `Whole_feature_reviewers
  | `Only    of User_name.Set.t
  | `All_but of User_name.Set.t
  ]
[@@deriving bin_io, compare, sexp]

include Equal.S with type t := t

val add : t -> User_name.Set.t -> whole_feature_reviewers:User_name.Set.t -> t

val mem
  :  t
  -> User_name.t
  -> whole_feature_reviewers : User_name.Set.t
  -> whole_feature_followers : User_name.Set.t
  -> is_seconded:bool
  -> bool

val to_sexp_hum : t -> Sexp.t

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
