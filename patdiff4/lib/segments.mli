open! Core
open! Import

type t = Segment.t list
[@@deriving sexp_of]

val of_files
  :  ?verbose:bool
  -> ?force_should_split_files_in_hunks_for_tests:bool
  -> rev_names: string Diamond.t
  -> context:int
  -> lines_required_to_separate_ddiff_hunks:int
  -> contents:string Diamond.t
  -> unit
  -> t
