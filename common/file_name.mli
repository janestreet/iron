(** Just a file name, not a full path.

    A file name is a nonempty string and is not allowed to contain a '/' or '\000'. *)

open! Core
open! Import

type t

include Identifiable with type t := t
include Invariant.S  with type t := t

val alphabetic_compare : t -> t -> int

(** [default_review_compare] is like [alphabetic_compare], but has domain-specific rules
    based on file suffixes, so that e.g. [foo.mli] appears before [foo.ml]. *)
val default_review_compare : t -> t -> int

(** Special Unix filenames *)
val dot    : t
val dotdot : t

val dot_fe : t

val scaffold_sexp : t

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end

