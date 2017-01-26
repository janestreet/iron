(** Syntactic form in [.fe.sexp] files for specifying a set of files. *)

open! Core
open! Import

type t [@@deriving sexp]

val all_files : t

val files : File_name.t list -> t

val eval
  :  t
  -> universe : File_name.Set.t
  -> Error_context.t
  -> File_name.Set.t
