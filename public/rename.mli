open! Core
open! Import

module Action : sig
  type t =
    { from           : Iron.Feature_path.t
    ; to_            : Iron.Feature_path.t
    ; skip_gca_check : bool
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_command_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
