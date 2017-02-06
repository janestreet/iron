(** Find, load, parse & resolve [.fe/obligations-global.sexp] and
    [.fe/obligations-repo.sexp].

    [fe obligations] needs two global spec files to operate:

    - The global spec file [.fe/obligation-global.sexp].
    - The repo spec file, which is found at [$repo_root/.fe/obligation-repo.sexp].

    The two files are distinct to handle scaffolded repos.

    There are three ways to find the obligation-global.sexp file:

    1. If we passed in a filename ([obligations_global_file = Some filename]), use that.
    2. Search upward in the filesystem, starting at $repo_root, looking for one.
    3. Use a [$repo_root/scaffold.sexp] to get enough information to get the
    bits with an [hg cat].

    #3 is necessary for the hydra worker, which works with "naked" scaffolded child repos
    -- that is, not unpacked inside their parent repo. You only get #3 if #2 fails --
    people use nested repos without scaffold.sexp files, and even when there is a
    [scaffold.sexp] file, it may not actually reflect the actual revset of the parent
    repo, and even when it does, #3 involves running hg over ssh, and so can be slow.
*)

open! Core
open! Async
open! Import

(** Fields are ordered following a topologic ordering of dependency. *)
type t = private
  { users                   : Unresolved_name.Set.t
  ; groups                  : Groups.t
  ; tags                    : Tag.Set.t
  ; scrutinies              : Scrutiny.t Scrutiny_name.Map.t
  ; build_projections       : Build_projection.t Build_projection_name.Map.t
  ; disallow_useless_dot_fe : bool
  ; allow_review_for        : Allow_review_for.t
  ; obligations_version     : Obligations_version.t
  }
[@@deriving sexp_of]

module Defined_in_local_repo : sig
  (** This is used to run some additional validation during [fe obligations check].  Does
      not need to be carried away past the validation. *)
  type t = private
    { tags : Tag.Set.t
    }
  [@@deriving sexp_of]
end

val load
  :  (module Hg_dependency.S)
  -> Repo_root.t
  -> aliases:User_name_by_alternate_name.t
  -> (t * Defined_in_local_repo.t) Or_error.t Deferred.t

module Stable : sig
  module V5 : Stable_without_comparator with type t = t
end
