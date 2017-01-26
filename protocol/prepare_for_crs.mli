open! Core
open! Import

module Action : sig
  type t =
    { feature_path : Feature_path.t
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t =
    { owner_for_crs   : User_name.t
    ; alternate_names : User_name_by_alternate_name.t
    ; aliases         : User_name_by_alternate_name.t
    }
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
