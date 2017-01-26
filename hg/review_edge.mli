open! Core
open! Import

type t =
  { base : Rev.t
  ; tip  : Rev.t
  }
[@@deriving sexp_of]

include Comparable.S with type t := t
include Hashable.S   with type t := t

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
