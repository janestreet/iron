open! Core
open! Import

type t =
  { bookmark        : string
  ; first_12_of_rev : Node_hash.First_12.t
  }
[@@deriving fields, sexp_of]

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
