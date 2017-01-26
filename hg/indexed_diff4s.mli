open! Core
open! Import

type t =
  { by_input  : Diff4.t list Diff2.Ignoring_rev.Table.t
  ; by_output : Diff4.t list Diff2.Ignoring_rev.Table.t
  }
[@@deriving fields, sexp_of]

include Invariant.S with type t := t

val create : Diff4.t list -> t
