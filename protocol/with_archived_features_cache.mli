open! Core
open! Import

module Action : sig
  type t =
    | Clear        of [ `All | `Feature_id of Feature_id.t ]
    | Set_max_size of int
  [@@deriving sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action = Action.t
  with type reaction = Reaction.t
