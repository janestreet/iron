open! Core
open! Async
open! Import

type t

val create  : string -> t
val message : t -> string

val get_non_temporary_message : t -> string
val get_temporary_message     : t -> string option

val set_temporary_message   : t -> expires_in:Time.Span.t -> string -> t
val clear_temporary_message : t -> t

val prod_path     : Abspath.t
val roll_etc_path : Abspath.t

val load_exn : Abspath.t -> t Deferred.t
val save_exn : t -> perm:Unix.file_perm -> Abspath.t -> unit Deferred.t
