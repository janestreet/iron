open! Core
open! Import

module Action : Unit

module Reaction : sig
  type t = Rpc_description.t list
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
