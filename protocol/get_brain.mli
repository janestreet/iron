open! Core
open! Import

module Action : sig
  type t =
    { feature_path : Feature_path.t
    ; for_         : User_name.t
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t =
    { reviewer         : Reviewer.t
    ; brain            : Brain.t
    ; remote_rev_zero  : Rev.t
    ; remote_repo_path : Remote_repo_path.t
    }
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
