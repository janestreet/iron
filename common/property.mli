open! Core
open! Import

type t = string [@@deriving sexp_of]

type comparator_witness = String.comparator_witness

include Comparable.S_plain with type t := t and type comparator_witness := comparator_witness
include Hashable  .S_plain with type t := t

include Stringable.S with type t := t

module Stable : sig
  module V1 : sig
    type t = string
    include Stable_without_comparator
      with type t := t
    include Comparable.Stable.V1.S
      with type comparable := t
      with type comparator_witness = comparator_witness
  end
end
