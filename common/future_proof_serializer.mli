(** A future proof serializer is a value that express how to encode / decode a value of
    type ['a] in a sexp such that it has both forward and backward compatibility
    properties. *)



open! Core

type 'a t

val of_sexp : 'a t -> Sexp.t -> 'a
val to_sexp : 'a t -> 'a -> Sexp.t

(** Combinators to build a value of type ['a t] *)
type ('load, 'save, 'r) map_args
val nil : ('a, 'a -> unit, 'a) map_args
val ( & )
  : 'a t
  -> ('load, 'save, 'r) map_args
  -> ('a -> 'load, ('a -> unit) -> 'save, 'r) map_args
val mapN : ('load, 'save, 'r) map_args -> load:'load -> save:'save -> 'r t

type 'a sexpable = (module Sexpable with type t = 'a)

val field     : string -> 'a sexpable -> default:'a -> 'a t
val field_opt : string -> 'a sexpable -> 'a option t
