open! Core
open! Import

module Which_user_info : sig
  type t =
    | Alias
    | Existing_user
    | Typo
  [@@deriving sexp_of]
end

module Type : sig
  type t =
    | Absolute_feature_path
    | Archived_feature_path
    | Feature_path
    | Feature_path_with_catch_up
    | Metric_name
    | Remote_repo_path
    | Root_feature_path
    | User_info of Which_user_info.t
  [@@deriving sexp_of]
end

module Action : sig
  type t =
    { types  : Type.t list
    ; prefix : string
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t = string list [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
