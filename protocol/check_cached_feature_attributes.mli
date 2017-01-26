open! Core
open! Import

module Action : sig
  type t =
    { which_features         : Which_features.t
    ; ignore_diffs_in_errors : bool
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
