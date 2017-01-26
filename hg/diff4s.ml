open! Core
open! Import

type t = Diff4.And_output_num_lines.t list [@@deriving sexp_of]

let invariant t = List.iter t ~f:Diff4.And_output_num_lines.invariant
