open! Core
open! Import

module Action : sig
  type t = Maybe_archived_feature_spec.t
  [@@deriving sexp_of]
end

module Reaction : sig
  type t =
    { description : string
    }
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
