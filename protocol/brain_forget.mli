open! Core
open! Import

module Action :sig
  type t =
    { feature_path   : Feature_path.t
    ; for_           : User_name.t
    ; what_to_forget : [ `All
                       | `Files of Path_in_repo.t list
                       ]
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t

