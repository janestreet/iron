open! Core
open! Import

type t = private
  { feature_id   : Feature_id.t
  ; feature_path : Feature_path.t
  ; rev_zero     : Rev.t
  ; owners       : User_name.t list
  ; archived_at  : Time.t
  }
[@@deriving compare, sexp_of]

include Invariant.S with type t := t

val create : Feature.t -> archived_at:Time.t -> t

(** Accessors. *)
val archived_at  : t -> Time.t
val feature_id   : t -> Feature_id.t
val feature_path : t -> Feature_path.t
val rev_zero     : t -> Rev.t
val owners       : t -> User_name.t list

val to_list_protocol : t -> Iron_protocol.List_features.Reaction.one

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
