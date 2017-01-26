open! Core
open! Import

module Action : sig
  type t =
    { rev_zero : Rev.t
    ; features : Feature_path.t list
    ; subtrees : Feature_path.t list
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type one =
    { feature_path : Feature_path.t
    ; base         : Rev.t
    ; tip          : Rev.t
    }
  [@@deriving sexp_of]

  type t =
    { remote_repo_path : Remote_repo_path.t
    ; features         : one list
    }
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
