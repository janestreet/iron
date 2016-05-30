open! Core.Std
open! Import

module Action : sig
  type t =
    { feature_path : Feature_path.t
    ; for_or_all   : [ `All_users | `User of User_name.t ]
    ; reason       : [ `Not_supported | `This of string ]
    }
  [@@deriving sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
