open! Core
open! Import

module Action : sig
  type t =
    | Satellites_only
    | Feature of Iron.Feature_path.t
  [@@deriving sexp_of]
end

module Reaction : Unit

include Iron_command_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
