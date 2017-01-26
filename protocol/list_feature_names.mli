open! Core
open! Import

module Action : sig
  type t =
    { descendants_of : Which_ancestor.t
    ; depth          : int
    ; use_archived   : bool
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
