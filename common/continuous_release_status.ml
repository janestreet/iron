open! Core
open! Import

type t =
  [ `Not_working_on_it
  | `Pending_or_working_on_it
  ]
[@@deriving compare, sexp_of]

let equal = [%compare.equal: t]
