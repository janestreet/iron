open! Core
open! Import

module Action : Unit

module Reaction : sig
  type one =
    { root_feature     : Feature_name.t
    ; remote_repo_path : Remote_repo_path.t
    ; tip              : Rev.t
    }
  [@@deriving fields, sexp_of]

  type t = one list
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
