open! Core
open! Async
open! Import

type t =
  | Static  of int
  | Dynamic of string (** file *)
[@@deriving sexp]

val port : t -> int Deferred.t
val write_if_dynamic : t -> port:int -> unit Deferred.t
