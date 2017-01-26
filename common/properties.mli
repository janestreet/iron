open! Core
open! Import

type t = Sexp.t Property.Map.t
[@@deriving sexp_of]

include Invariant.S with type t := t

val empty : t

val to_rows : t -> (string * string) list

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
