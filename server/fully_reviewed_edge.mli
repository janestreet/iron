open! Core
open! Import

module Reason : sig
  type t =
    | External of unit Query.t
    | Release of Feature_path.t * Rev.t
    | Review
  [@@deriving sexp_of]
end

type t =
  { rev_zero : Rev.t
  ; from_    : Rev.t
  ; to_      : Rev.t
  ; reason   : Reason.t
  }
[@@deriving fields, sexp_of]

include Invariant.S  with type t := t

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
