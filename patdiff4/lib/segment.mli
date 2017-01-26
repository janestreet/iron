open! Core
open! Import

type t =
  { slice       : Slice.t Diamond.t
  ; diff4_class : Diff4_class.t
  }
[@@deriving sexp_of]

val is_shown : t -> bool

val prepend : string list -> t -> t
val append  : t -> string list -> t
