open! Core
open! Import

type ('syntax, 'sexp) t =
  { syntax : 'syntax
  ; sexp   : 'sexp option
  }

let sexp_of_t sexp_of_syntax _ t = t.syntax |> [%sexp_of: syntax]

let t_of_sexp syntax_of_sexp sexp_of_sexp sexp =
  { syntax = sexp       |> [%of_sexp: syntax]
  ; sexp   = Some (sexp |> [%of_sexp: sexp])
  }
;;

let create syntax = { syntax; sexp = None }

let creates syntaxes = List.map syntaxes ~f:create

