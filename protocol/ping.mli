open! Core.Std
open! Import

module Action   : Unit
module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
