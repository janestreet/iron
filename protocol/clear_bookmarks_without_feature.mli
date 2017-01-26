open! Core
open! Import

module Action : sig
  type t =
    { remote_repo_path : Remote_repo_path.t
    }
  [@@deriving fields,  sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
