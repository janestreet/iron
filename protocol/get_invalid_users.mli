open! Core
open! Import

module Action : Unit

module Reaction : sig
  type t = User_name_occurrence.t list User_name.Map.t
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
