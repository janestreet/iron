open! Core.Std
open! Import

module Namespace : sig
  type t = Maybe_archived_feature_spec.Namespace.t
  [@@deriving sexp_of]
end

module Action : sig
  type t =
    { partial_name_prefix : string
    ; namespace           : Namespace.t
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t =
    { matching_features : Feature_path.t list
    ; rev_zero_roots    : (Feature_name.t * Rev.t) list
    }
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
