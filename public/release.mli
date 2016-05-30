open! Core.Std
open! Import

module Included_features_order : sig
  type t =
    [ `Name
    | `Release_order
    ]
  [@@deriving sexp_of]
end

module Action : sig
  type t =
    { feature_path            : Iron.Feature_path.t
    ; for_                    : Iron.User_name.t
    ; included_features_order : Included_features_order.t
    }
  [@@deriving sexp_of]
end

module Reaction : Unit

include Iron_command_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
