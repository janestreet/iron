open! Core
open! Import

module Action : sig
  type t =
    | Clear_features        of Which_features.t
    | Clear_revs            of Rev.t list
    | Set_max_size          of int
    | Set_status            of Worker_cache.Status.t
    | Set_max_items_per_rpc of int
  [@@deriving sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
