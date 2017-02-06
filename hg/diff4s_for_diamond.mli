open! Core
open! Async
open! Import

(** This is a cache of hg commands for a particular repository.  It is there to allow
    sharing some of the hg/diffing work between several invocations of [create] below.
    The list of diamonds it receives should include all the diamonds [create] is fed for
    that cache. *)
module Cache : sig
  type t

  val with_
    :  ?time:(string -> unit)
    -> Repo_root.t
    -> Obligations.t Rev.Compare_by_hash.Map.t
    -> Rev.t Diamond.t list
    -> f:(t -> 'a Deferred.t)
    -> 'a Deferred.t
end

val create
  :  Cache.t
  -> Rev.t Diamond.t
  -> lines_required_to_separate_ddiff_hunks:int
  -> Diff4.t list Deferred.t

val create_using_fake_obligations
  :  Repo_root.t
  -> Raw_rev.t Diamond.t
  -> lines_required_to_separate_ddiff_hunks:int
  -> Diff4.t list Or_error.t Deferred.t

val command : Command.t
