open! Core
open! Import

type +'a t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

(** Accessors. *)
val reaction : 'a t -> 'a

val create
  :  'a
  -> query_uuid               : Uuid.t
  -> server_received_query_at : Time.t
  -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val server_took : _ t -> Time.Span.t

module Stable : sig
  module V1 : Stable1 with type 'a t = 'a t
end
