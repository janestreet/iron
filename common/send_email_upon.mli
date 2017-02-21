open! Core
open! Import

type t =
  | Archive
  | Release
[@@deriving sexp_of]

include Comparable.S_plain with type t := t
include Enum.S             with type t := t
include Equal.S            with type t := t
include Stringable         with type t := t

val default : Set.t

module Stable : sig
  module V1 : sig
    include Stable_without_comparator with type t = t
    include Core_kernel.Comparable.Stable.V1.S
      with type comparable := t
      with type comparator_witness := comparator_witness
  end
end
