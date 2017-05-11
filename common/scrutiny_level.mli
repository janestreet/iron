(** The level of attention a reviewer should have when reviewing a file.

    A scrutiny level is an integer between zero and 100, inclusive.  A higher number
    requires a higher level of attention.  Scrutiny levels are defined in
    [obligations-global.sexp]. *)

open! Core
open! Import

type t [@@deriving sexp_of]

include Comparable.S with type t := t
include Invariant.S  with type t := t

val hash : t -> int

val of_int : int -> t
val to_int : t -> int

val ignored : t (** Zero -- this scrutiny level means "ignore the file." *)

val to_string_hum : t -> string

module Syntax : sig
  type nonrec t = t [@@deriving compare, sexp]
end

module Stable : sig
  module V1 : sig
    include Stable_without_comparator with type t = t
    val hash : t -> int
  end
end
