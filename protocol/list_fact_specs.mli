open! Core
open! Import

module Action : Unit

module Reaction : sig
  type t = (Fact.Spec.Id.t * Fact.Spec.t) list [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
