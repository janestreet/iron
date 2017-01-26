open! Core

type t =
  | V1
  | V2_sql_xml
[@@deriving sexp]

include Equal.S with type t := t

val latest : t

val is_after_format : t -> format:t -> bool
