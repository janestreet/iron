open! Core
open! Import

module Elt : sig
  type 'a t
  val value : 'a t -> 'a
  val index : _ t -> int
end

type 'a t

val length : _ t -> int

val create          : 'a list -> 'a t

val current         : 'a t -> 'a Elt.t option
val to_list         : 'a t -> 'a Elt.t list
val is_empty        : _ t -> bool

val goto_next       : _ t -> unit
val goto_previous   : _ t -> unit

val delete : 'a t -> 'a Elt.t -> unit

val values : 'a t -> 'a list

val goto : 'a t -> 'a Elt.t -> unit Or_error.t
