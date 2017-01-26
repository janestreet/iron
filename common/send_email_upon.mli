open! Core
open! Import

type t [@@deriving sexp_of]

include Comparable with type t := t
include Enum.S     with type t := t
include Equal.S    with type t := t
include Stringable with type t := t

val archive : t
val release : t

val default : Set.t

module Stable : sig
  module V1 : sig
    include Stable_without_comparator with type t = t
    include Core_kernel.Std.Comparable.Stable.V1.S
      with type comparable := t
      with type comparator_witness := comparator_witness
  end
end
