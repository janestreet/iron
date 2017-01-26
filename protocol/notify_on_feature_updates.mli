open! Core
open! Import

module Action : sig
  type t =
    { feature_id           : Feature_id.t
    ; when_to_first_notify : When_to_first_notify.t
    }
  [@@deriving sexp_of]
end

module Reaction : sig
  type t = [ `Updated of Feature.t
           | `Archived
           ]
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S_pipe_rpc
  with type action   = Action.t
  with type reaction = Reaction.t
