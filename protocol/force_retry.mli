open! Core
open! Import

module Action : sig
  type t = Feature_path.t
  [@@deriving sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
