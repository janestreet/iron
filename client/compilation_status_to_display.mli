open! Core
open! Import

type t [@@deriving sexp_of]

val of_compilation_status
  :  Compilation_status.t
  -> Feature_path.t
  -> feature_tip:Rev.t
  -> show_full_compilation_status:bool
  -> t

val to_ascii_table_rows : t -> (string * (Ascii_table.Attr.t list * string)) list
