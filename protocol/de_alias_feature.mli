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
    { de_aliased                                         : User_name.Set.t
    ; did_not_de_alias_due_to_review_session_in_progress : User_name.Set.t
    ; nothing_to_do                                      : User_name.Set.t
    }
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
