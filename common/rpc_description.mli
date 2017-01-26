open! Core
open! Async.Std
open! Import

type t = Rpc.Description.t =
  { name    : string
  ; version : int
  }
[@@deriving fields, sexp_of]

include Comparable with type t := t

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end

val to_ascii_table : t list -> Iron_ascii_table.t
