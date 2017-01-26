open! Core
open! Import

module Action : sig
  type t =
    { feature_path                           : Feature_path.t
    ; add_whole_feature_reviewers            : User_name.Set.t option
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
