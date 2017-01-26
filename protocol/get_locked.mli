open! Core
open! Import

module Action : sig
  type t =
    { feature_path : Feature_path.t
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t = (Lock_name.t * Feature.Locked.t list) list
  [@@deriving sexp_of]

  (** [sort] is exposed so that it can be done client side. *)
  val sort : t -> t
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
