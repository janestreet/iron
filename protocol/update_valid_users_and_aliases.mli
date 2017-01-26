open! Core
open! Import

module User_aliases : sig
  type t =
    { user_name           : User_name.t
    ; aliases             : Alternate_name.t list
    }
  [@@deriving sexp_of]
end

module Action : sig
  type t =
    { valid_users_and_aliases : User_aliases.t list
    ; may_repartition_crs     : bool
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
