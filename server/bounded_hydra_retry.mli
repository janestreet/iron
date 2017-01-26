(** This datatype is used to limit how many times Iron can asked hydra to retry a
    bookmark, to avoid making hydra keep handling the same bookmark over and over
    again. *)

open! Core
open! Import

type t [@@deriving sexp_of, compare]

include Invariant.S with type t := t

val empty : t

val force_next_time : t -> t

val should_force : t -> t option

val can_retry : t -> t option
