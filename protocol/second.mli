open! Core
open! Import

module Action : sig
  type t =
    { feature_path      : Feature_path.t
    ; even_though_empty : bool
    ; even_though_owner : bool
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
