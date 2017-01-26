open! Core
open! Import

module Action : sig
  type t = (Fact.Spec.Id.t * Fact.Scope.t) [@@deriving sexp_of]
end

module Reaction : sig
  type t = Fact.Evidence.t [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
