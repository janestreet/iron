open! Core
open! Import

module Action : sig
  type t =
    { from_                  : Feature_path.t
    ; to_                    : Feature_path.t
    ; rev_zero               : Rev.t
    ; without_copying_review : bool
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t =
    { feature_id       : Feature_id.t
    ; remote_repo_path : Remote_repo_path.t
    ; tip              : Rev.t
    }
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
