open! Core
open! Import

module Action : sig
  type t =
    { feature_path : Feature_path.t
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t = Cr_comment.Summary.t Or_error.t
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
