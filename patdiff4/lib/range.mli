open! Core.Std

type t = {
  source     : string; (* Represents the file where a range came from. *)
  line_start : int;    (* inclusive *)
  line_end   : int;    (* exclusive *)
} [@@deriving sexp_of, compare, fields]

val merge : t -> t -> t
val to_header : other_names:string list -> t -> Header.Source.t

val prepend : int -> t -> t
val append  : t -> int -> t
