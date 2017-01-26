open! Core
open! Import

module Action : sig
  type t =
    { feature_path            : Iron.Feature_path.t
    ; feature_id              : Iron.Feature_id.t option
    }
  [@@deriving sexp_of]
end

module Reaction : Unit

include Iron_command_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
