open! Core.Std
open! Import

module Action : sig
  type t =
    { feature_path : Feature_path.t option
    ; depth        : int
    ; use_archived : bool
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t = Feature_path.t list
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
