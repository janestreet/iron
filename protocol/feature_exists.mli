open! Core
open! Import

module Action : sig
  type t = Feature_path.t [@@deriving sexp_of]
end

module Reaction : sig
  type t =
    | No
    | Yes of Feature_id.t
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
