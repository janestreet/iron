open! Core.Std
open! Import

type t = Segment.t list
[@@deriving sexp_of]

val of_files
  :  ?verbose:bool
  -> ?force_should_split_files_in_hunks:bool
  -> rev_names: string Diamond.t
  -> context:int
  -> contents:string Diamond.t
  -> unit
  -> t
