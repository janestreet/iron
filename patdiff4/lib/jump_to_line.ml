open Core.Std

(* The line number of the merged file that we need to jump to in emacs. *)
type t = int [@@deriving sexp_of]
