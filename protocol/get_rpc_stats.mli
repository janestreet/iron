open! Core
open! Import

module Action : Unit

module Key : sig
  type t =
    { by          : User_name.t
    ; rpc_name    : string
    ; rpc_version : int
    }
  [@@deriving compare, fields, sexp_of]

  val hash : t -> int
end

module Data : sig
  type t =
    { hits : int
    ; took : Time.Span.t
    }
  [@@deriving fields, sexp_of]
end

module Reaction : sig
  type t = (Key.t * Data.t) list
  [@@deriving sexp_of]
end

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t
