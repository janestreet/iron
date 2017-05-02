open! Core
open! Import

type t =
  { feature_id           : Feature_id.t
  ; feature_path         : Feature_path.t
  ; rev_zero             : Rev.t
  ; owners               : User_name.t list
  ; archived_at          : Time.t
  ; reason_for_archiving : string
  }
[@@deriving fields, sexp_of]

include Invariant.S with type t := t

val create : Feature.t -> archived_at:Time.t -> reason_for_archiving:string -> t

val to_list_protocol : t -> Iron_protocol.List_features.Reaction.one

val to_is_archived : t -> Is_archived.t
