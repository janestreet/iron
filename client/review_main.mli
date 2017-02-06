open! Core
open! Async
open Pdiff4.Std
open! Import

module type M = sig
  type t
  val reviewed                  : t list -> unit Deferred.t
  val path_in_repo              : t -> Path_in_repo.t
  val num_lines_in_diff         : (t -> int) option
  val always_open_file_in_emacs : bool
  val open_file_in_emacs        : (t -> unit Deferred.t) option
  val may_commit_session        : bool
end

val files
  :  (module M with type t = 'a)
  -> ('a * (Hunk.t list Or_error.t Deferred.t)) list
  -> [ `Reviewed | `Commit_session | `Quit ] Deferred.t

val hunks_to_lines : Hunk.t list -> string list
