(** The root of an hg repository that is expected to exist on the filesystem.

    The fe server never creates such a value, so it cannot run any functions in this
    library that calls out to hg, since they all take such a value. *)

open! Core
open! Import

type t [@@deriving compare, sexp_of]

module Hash_by_path : Hashable.S_plain with type t = t

(** [human_readable] might be used for example to talk about satellites repos in a
    scaffold.  It helps producing better messages than using the raw directory names. *)
val of_abspath : ?human_readable:string -> Abspath.t -> t
val to_abspath : t -> Abspath.t

val to_string : t -> string

(** Defaults to [to_string] if no human readable name is set *)
val to_string_hum      : t -> string

val with_human_readable : t -> string -> t

(** Find the containing repo root -- the closest super directory with an [.hg/] entry. *)
val containing_root_blocking : ?human_readable:string -> Abspath.t -> t Or_error.t

val program_started_in : t Or_error.t

val relativize_exn : t -> Abspath.t -> Path_in_repo.t

val append : t -> Path_in_repo.t -> Abspath.t

val have_same_abspath : t -> t -> bool
