(** The scrutiny of a particular file.  It doesn't contain information like the
    description of the scrutiny, but only what is relevant to review a particular file.
    File scrutinies are compared by their levels, but displayed using their names. *)

open! Core
open! Import

type t [@@deriving sexp_of]

include Comparable.S with type t := t

val create : Scrutiny_name.t -> Scrutiny_level.t -> t
val to_string_hum : t -> string
val ignored : t

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
