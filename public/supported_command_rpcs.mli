open! Core
open! Import

module Action : Unit

module Reaction : sig
  type t = Iron.Rpc_description.t list
end

include Iron_command_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
