(** The meaning of a [scaffold.sexp] file, which defines scaffold-repo structure.

    A scaffolded repo consists of a "center" repo, which contains [scaffold.sexp] at its
    root, and a number of "satellite" repos, which are mentioned in [scaffold.sexp].  The
    clone of a scaffolded repo consists of clones of the center and satellites, all nested
    within one of the satellites (called the "enclosing repo root").

    A [Scaffold.t] is a representation of the relative paths in a nested set of
    repositories; it does not itself refer to any absolute path on the file system.

    On the file system, a scaffolded repo can exist in isolation, in which case there is
    just a center repo.  Given the center repo root, a [Scaffold.t] is sufficient to
    locate all of the satellites.
*)

open! Core
open! Async
open! Import

module Satellite : sig
  (** [repo_root] is relative to enclosing root of the scaffolded structure.
      [human_readable] is a name in the context of the scaffold structure.  [revision] is
      resolved later, when we have access to a repo.  It can be a global tag or 40-char
      rev. *)
  type t = private
    { repo_root        : Relpath.t
    ; remote_repo_path : Remote_repo_path.t
    ; human_readable   : string
    ; revision         : string
    }
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val center_relative_to_enclosing_repo : t -> Relpath.t

(** [satellites] is sorted alphabetically by [repo_root]. *)
val satellites : t -> Satellite.t list

val load_scaffold_file_exn : Abspath.t -> t Deferred.t
val parse_scaffold_contents_exn : string -> t

(** [load] returns [None] if there is no scaffold.  [load] raises if the scaffold is
    erroneous. *)
val load : center_repo_root:Repo_root.t -> t option Deferred.t

module Center_relative_to_enclosing_repo : sig
  (** We keep a file at the root of the enclosing repo to keep track of where the root of
      the center repo is relative to it. *)
  val load_exn : enclosing_repo_root_abspath:Abspath.t -> Relpath.t Deferred.t
  val save_exn : enclosing_repo_root_abspath:Abspath.t -> Relpath.t -> unit Deferred.t
end

val find_enclosing_repo_root
  :  t
  -> center_repo_root:Repo_root.t
  -> [ `Enclosing_repo_root of Repo_root.t
     | `Isolated_repo_has_no_enclosing_repo_root
     ] Deferred.t

(** [update_satellite_repos] does nothing if [center_repo_root] is isolated. *)
val update_satellite_repos : t -> center_repo_root:Repo_root.t -> unit Deferred.t
