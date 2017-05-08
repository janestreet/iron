open! Core
open! Import

type t =
  [ `Not_working_on_it
  | `Pending_or_working_on_it
  ]
[@@deriving sexp_of]

include Equal.S with type t := t
