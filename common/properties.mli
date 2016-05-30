open! Core.Std
open! Import

type t = Sexp.t String.Table.t
[@@deriving sexp_of]

include Invariant.S with type t := t

val to_rows : t -> (string * string) list

val create : unit -> t

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
