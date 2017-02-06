(** For saving state to a subdirectory tree of the file system.

    The semantics is a sequence of operations to the file system.  Async is used
    underneath to do the file-system side effecs.

    One can think of a serializer as a capability that allows one to write to a subtree
    of the file system.
*)

open! Core
open! Async
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

(** Accessors *)
val root_directory   : t -> Abspath.t
val relative_to_root : t -> Relpath.t

val do_nothing : t

(** All side effects will take place under [root_directory]. *)
val create
  :  ?should_do_effects:bool  (** default is [true] *)
  -> root_directory:Abspath.t
  -> unit
  -> t

(** [relativize t path] returns a serializer [t2] such that:

    {[
      relative_to_root t2 = Relpath.append (relative_to_root t1) path
    ]}

    and [t2] has the cache invalidators of [t1] at the time of creation.
*)
val relativize : t -> dir:Relpath.t -> t

(** Attach a cache invalidator if not already present.  Any subsequent write operation
    performed on the serializer will invalidate all dependents of all present
    invalidators. *)
val add_cache_invalidator : t -> Cached.Invalidator.t -> unit
val invalidate_dependents : t -> unit

val add_subtree    : t -> dir:Relpath.t                           -> unit
val remove_subtree : t -> dir:Relpath.t                           -> unit
val append_to      : t -> file:Relpath.t -> 'a -> 'a Persistent.Writer.t -> unit
val rename         : t -> from_:Relpath.t -> to_:Relpath.t -> unit
val set_contents   : t -> file:Relpath.t -> 'a -> 'a Persistent.Writer.t -> unit

(** [uncompress_subtree_if_needed ~dir] uncompresses the file named "${dir}.tar.xz", which
    must result in a directory named ${dir} (and will if using [compress_subtree]). The
    archive is deleted after a successful uncompression.

    Consider the following conditions:

    A: ${dir}         exists
    B: ${dir}.tar.xz  exists

    if A && not B then the function call is a no-op.  if A && B, then ${dir} is deleted
    and the archived file is uncompressed (and then deleted). *)
val uncompress_subtree_if_needed : t -> dir:Relpath.t -> unit

(** [compress_subtree_if_needed] is mirror what is done in
    [uncompress_subtree_if_needed] in reverse direction *)
val compress_subtree_if_needed : t -> dir:Relpath.t -> unit

val clear_sequence : t -> file:Relpath.t -> unit

val prior_changes_synced_to_file_system : t -> unit Deferred.t

(** [pause_exn] returns a deferred that becomes determined when all previously enqueued
    changes to the file system have completed, and then prevents any subsequent enqueued
    changes from happening until a subsequent call to [resume].

    If [resume_exn] is not called within [with_timeout], then the serializer is forcibly
    resumed.

    Pausing is useful for backups, which temporarily pause serialization so that they can
    snapshot a consistent state. *)
val pause_exn
  :  t
  -> query        : unit Query.t
  -> with_timeout : Time.Span.t
  -> unit Deferred.t

val resume_exn   : t -> unit

(** [pause_status t] returns a description intended for humans of whether [t] is paused or
    not. *)
val pause_status : t -> Sexp.t
