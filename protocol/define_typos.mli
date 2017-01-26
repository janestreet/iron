open! Core
open! Import

module Definition : sig
  type t =
    { typo  : Alternate_name.t
    ; means : User_name.t
    }
  [@@deriving fields, sexp_of]
end

module Action : sig
  type t =
    { definitions         : Definition.t list
    ; may_repartition_crs : bool
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
