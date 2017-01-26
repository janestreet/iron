module type S = sig
  open! Core
  open! Import

  type t [@@deriving sexp_of]

  include Comparable.S with type t := t
  include Hashable.S   with type t := t
  include Invariant.S  with type t := t

  val create   : string -> t

  val of_empty_string : t

  module Stable : sig
    module V1 : Stable_without_comparator with type t = t
  end
end
