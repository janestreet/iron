open! Core

(** A header is just some text that looks something like:

    @@@@@@@ -name 10,20 +name 20,30 @@@@@@@@ *)
module Source : sig
  type t =
    { name        : string
    ; other_names : string list
    ; range       : (int * int) option
    }
end

type 'a diff =
  { minus : 'a
  ; plus  : 'a
  }

type t =
  | Diff2 of Source.t diff
  | Diff4 of Source.t diff diff

val add_hunk_break : t -> (int*int) -> (int*int) -> t

val separator : string
val filename_header : filename:string -> int * string
val filename_separator : length:int -> string

val errors : string
val forget : string

val to_string : t -> string list

module Hint : sig
  val b1_b2_f2      : string
  val b1_f1_f2      : string
  val b1_f2__b2_f1  : string
  val b1_f2         : string

  val b2_f2         : string
  val b2_f2_story   : string * string
  val b2_f2_dropped : string
  val b2_f2_kept    : string

  val f1_f2         : string
  val f1_f2_story   : string * string
  val f1_f2_dropped : string
  val f1_f2_kept    : string
end

val title : string -> string
