open! Core
open! Import

(** Return a pipe of events reflecting each time a feature is modified on the server in a
    feature's subtree. Descendants include the feature itself.  When [Archived] or
    [Renamed] are encountered, these are the last value sent in the pipe because the
    subtree watched is gone. *)

module Action : sig
  type t =
    { feature_path         : Feature_path.t
    ; when_to_first_notify : When_to_first_notify.t
    }
  [@@deriving sexp_of]
end

module Reaction : sig
  type t =
    [ `Updates_in_subtree
    | `Archived
    | `Renamed
    ]
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S_pipe_rpc
  with type action   = Action.t
  with type reaction = Reaction.t
