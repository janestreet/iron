open! Core

type t =
  | V1
  | V2
  | V3
  | V4
[@@deriving sexp]

val default : t
val latest : t

val is_at_least_version : t -> version:t -> bool

val cr_comment_format : t -> Cr_comment_format.t

module Stable : sig
  module V1 : sig
    include Stable_without_comparator with type t = t
    val hash : t -> int
  end
end
