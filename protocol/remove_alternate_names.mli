open! Core
open! Import

module Action : sig
  type t =
    { alternate_names     : Alternate_name.t list
    ; which               : [ `Aliases | `Typos ]
    ; may_repartition_crs : bool
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
