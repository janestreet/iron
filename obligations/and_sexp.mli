(** [And_sexp] is for storing the original sexp that was deserialized to produce a syntax
    tree.  When processing the syntax tree, the sexp can be used to determine the
    source-code position that caused an error.

    ['sexp] is either [Sexp.t] or [Sexp.Annotated.t].

    [t_of_sexp sexp] stores [sexp] in the resulting [t]. *)

type ('syntax, 'sexp) t =
  { syntax : 'syntax
  ; sexp   : 'sexp option
  }
[@@deriving sexp]

val create  : 'syntax      -> ('syntax, _) t
val creates : 'syntax list -> ('syntax, _) t list
