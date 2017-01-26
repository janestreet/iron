open! Core
open! Import

type t = Diff4.And_output_num_lines.t list [@@deriving sexp_of]

include Invariant.S with type t := t
