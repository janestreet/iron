(** These regexps are like regular [Re2] regexps, but they only match the entire string.
    We make them by wrapping the pattern string with [\A...\z] to anchor the match. *)

open! Core
open! Import

type t

(** [of_string] checks to ensure input is a valid regexp. *)
include Identifiable with type t := t

val matches : t -> string -> bool
val rewrite : t -> template:string -> string -> string Core.Or_error.t
val valid_rewrite_template : t -> template:string -> bool
