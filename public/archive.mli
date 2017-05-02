open! Core
open! Import

module Action : sig
  type t =
    { feature_path         : Iron.Feature_path.t
    ; for_                 : Iron.User_name.t
    ; reason_for_archiving : string
    }
  [@@deriving sexp_of]
end

module Reaction : Unit

include Iron_command_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
