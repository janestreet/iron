open! Core.Std

type t

(* Create some structured elisp (for interacting with emacs) out of a list of hunks. A
   hunk is diffed with a bunch of different diff algorithms, and there's a set of lines
   representing the output of each. *)
val create
  :  Hunk.Lines.t list list
  -> t

val cons : Hunk.Lines.t list -> t -> t

(* Converts an emacs_diff into an expression elisp understands. *)
val to_elisp : t -> string
