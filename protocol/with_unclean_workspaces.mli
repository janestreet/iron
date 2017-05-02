open! Core
open! Import

module Action : sig
  type t =
    | Remove_user    of User_name.t
    | Remove_machine of User_name.t * Machine_name.t
  [@@deriving sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
