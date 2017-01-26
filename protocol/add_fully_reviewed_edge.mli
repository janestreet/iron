open! Core
open! Import

module Feature_base_to_tip : sig
  type t =
    { feature_path               : Feature_path.t
    ; even_if_release_is_locked  : bool
    ; if_feature_is_empty        : [ `Do_nothing | `Fail ]
    } [@@deriving sexp_of]
end

module Action : sig
  type t =
    { rev_zero : Rev.t
    ; edge     : [ `Feature_base_to_tip of Feature_base_to_tip.t
                 | `From_to             of Rev.t * Rev.t
                 ]
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
