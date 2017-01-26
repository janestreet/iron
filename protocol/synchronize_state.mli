open! Core
open! Import

module Action : sig
  type t =
    { remote_repo_path : Remote_repo_path.t
    ; bookmarks        : Hydra_state_for_bookmark.t list
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t =
    { bookmarks_to_rerun : string list
    }
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
