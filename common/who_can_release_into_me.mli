open! Core
open! Import

type t =
  | My_owners
  | My_owners_and_child_owners
[@@deriving sexp_of]

include Enum.S     with type t := t
include Comparable with type t := t

val to_string_hum_as_parent : t -> string

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
