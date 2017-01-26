(** For processing abstract syntax trees and reporting error positions in the syntax tree,
    while distinguishing between errors in the tree (which are reported by [error] below)
    and exceptions in the processing code.

    One creates an error context using [within ~file f], where [f] processes the syntax
    tree, calling [error] for any errors in the tree.  As [f] descends the tree, it uses
    [augment] to produce new error contexts reflecting the syntax tree that is being
    processed.  When [error] is called, it searches for the [sexp] in the
    [annotated_sexp], and reports the corresponding position.  [within] uses [with_return]
    so that any call to [error] by [f] returns immediately to the [within].
*)

open! Core
open! Import

type t

val within : file:Path.t -> (t -> 'a) -> 'a Or_error.t

val augment
  :  ?annotated_sexp : Sexp.Annotated.t
  -> ?info           : Info.t
  -> ?sexp           : Sexp.t
  -> t
  -> t

val raise_f : t -> ('r, unit, string, unit -> _) format4 -> 'r
val raise_s : t -> Sexp.t -> _
val raise   : t -> Error.t -> _
