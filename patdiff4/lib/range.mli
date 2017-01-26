open! Core
open! Import

type t =
  { source     : string (** Represents the file where a range came from. *)
  ; line_start : int    (** inclusive *)
  ; line_end   : int    (** exclusive *)
  }
[@@deriving compare, fields, sexp_of]

val merge     : t -> t -> t
val to_header : other_names:string list -> t -> Header.Source.t

val prepend : int -> t -> t
val append  : t -> int -> t
