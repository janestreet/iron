open! Core
open! Import

module Action : sig
  type t =
    { from           : Feature_path.t
    ; to_            : Feature_path.t
    ; rev_zero       : Rev.t
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t =
    { from_feature_path      : Feature_path.t
    ; from_base              : Rev.t
    ; from_tip               : Rev.t
    ; to_parent_feature_path : Feature_path.t
    ; to_parent_tip          : Rev.t
    ; renames                : Rename.t list
    ; remote_repo_path       : Remote_repo_path.t
    }
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
