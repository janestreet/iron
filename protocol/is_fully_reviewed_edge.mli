open! Core
open! Import

module Action : sig
  type t =
    { from : Rev.t
    ; to_  : Rev.t
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
