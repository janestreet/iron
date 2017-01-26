open! Core
open! Import

type t = Continuous | Direct
[@@deriving compare, sexp_of]

include Enum.S  with type t := t
include Equal.S with type t := t

val to_string_hum : t -> string

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
