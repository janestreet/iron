open! Core
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val create : unit -> t

val add_hit : t -> Iron_protocol.Get_rpc_stats.Key.t -> took:Time.Span.t -> unit

val to_protocol : t -> Iron_protocol.Get_rpc_stats.Reaction.t
