open! Core

(** Frequently, instead of a full diff, we can just show a change from one set of lines to
    another. *)
type 'a t =
  { from : 'a
  ; to_  : 'a
  }
[@@deriving sexp_of, fields]

(*_ val map : 'a t -> f:('a -> 'b) -> 'b t *)
