open! Core
open! Import

module Action : sig
  type t =
    { feature_path : Feature_path.t
    ; whose_review : User_name.Or_all_or_all_but.t
    ; reason       : [ `Not_supported | `This of string ]
    }
  [@@deriving sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
