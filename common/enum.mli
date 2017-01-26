open! Core
open! Import

module type S = sig
  type t [@@deriving sexp_of]
  val all : t list
end

type 'a t = (module S with type t = 'a)

val to_string_hum : 'a t -> 'a -> string
