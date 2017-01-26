open! Core
open! Import

(** This is a single file's hunk. For the full 4-way hunk, see [Segment.t]. *)
type t =
  { range : Range.t
  ; lines : string list
  }
[@@deriving sexp_of, compare, fields]

(** Create a slice from a source, initial line number, and some lines. *)
val create : source:string -> int -> string list -> t

val prepend : string list -> t -> t
val append  : t -> string list -> t
