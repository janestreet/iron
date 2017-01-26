open! Core
open! Import

module Action : sig
  type t =
    [ `Aliases
    | `Typos
    | `All
    ]
  [@@deriving sexp_of]
end

module Reaction : sig
  type t = User_name_by_alternate_name.t [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
