open! Core
open! Async
open! Import

(** [sort repo_root xs path_in_repo] sorts [xs] in dependency order of their
    [path_in_repo] as determined by build artifacts in [repo_root].  The result is always
    a permutation of [xs].  The determination of dependency order may fail, and if so,
    [sort] does not raise, but rather prints a message to stderr and returns [xs],
    unchanged.

    The sorting first determines an order on directories, and then within each directory,
    an order on files.

    These build artifacts are used:

    - [libmap.sexp], for mapping library name to directory name.
    - [*.libdeps], which gives library dependencies, and is used for directory order.
    - [*.d], which gives module dependencies, and is used for file order.

    [sort] works in scaffolded repos, but of course then requires that [repo_root] be
    inside the scaffolded repo. *)
val sort
  :  (Repo_root.t * Cmd_workspace.Repo_root_kind.t) Or_error.t
  -> 'a list
  -> ('a -> Path_in_repo.t)
  -> 'a list Deferred.t

(** [staged_sort list] is like [sort list], except that the function it returns can be
    used to sort any subset of [list]. *)
val staged_sort
  :  (Repo_root.t * Cmd_workspace.Repo_root_kind.t) Or_error.t
  -> 'a list
  -> ('a -> Path_in_repo.t)
  -> ('a list -> 'a list) Deferred.t
