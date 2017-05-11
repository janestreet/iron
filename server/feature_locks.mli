(** All locks for a feature, at most one for each pair [User_name * Lock_name]. *)

open! Core
open! Import

module Locked : sig
  type t

  val query        : t -> unit Query.t
  val by           : t -> User_name.t
  val reason       : t -> string
  val is_permanent : t -> bool

  val to_protocol : t -> Iron_protocol.Feature.Locked.t
end

type t [@@deriving sexp_of]

include Invariant.S  with type t := t

(** [find t lock_name] returns an empty list if [lock_name] is not locked. *)
val find : t -> Lock_name.t -> Locked.t list

(** The resulting [Locked.t list]s will all be nonempty. *)
val what_is_locked : t -> (Lock_name.t * Iron_protocol.Feature.Locked.t list) list

val check_unlocked : t -> Lock_name.t -> unit Or_error.t

val check_all_unlocked : t -> unit Or_error.t

val is_permanently_locked : t -> Lock_name.t -> bool

val create : Feature_path.t -> t

val set_feature_path : t -> Feature_path.t -> unit

(** [lock] always succeeds.  If the lock is already taken by [for_] the lock information
    is overridden.  Users may take at most one lock for a given lock_name. *)
val lock
  :  t
  -> query        : _ Query.t
  -> for_         : User_name.t
  -> lock_name    : Lock_name.t
  -> reason       : string
  -> is_permanent : bool
  -> unit

(** [unlock] only returns [Ok] if the lock is locked, and was locked by [for_]. *)
val unlock
  :  t
  -> for_              : User_name.t
  -> lock_name         : Lock_name.t
  -> even_if_permanent : bool
  -> unit Or_error.t

(*_ The versioning of these types is done separately:

  - in [feature.ml] limited to [sexp], because it is much more flexible than [bin_io],
  and we will probably be able to get by using [sexp_option] and such.

  - in [protocol/lock_feature.ml], and we will decide what version we want to support
  there.  Could be different from the persistent versions kept alive.
*)
