(** Modules that links publisher of feature updates to their subscribers.

    Updates are declared by [on_archive] and related functions. We batch (and consolidate)
    updates until a call to [broadcast_updates]. [broadcast_updates] is called after each
    RPC, so RPCs can publish their changes as they make them, and trigger features just
    once at the end.
    However because a few RPCs are deferred, a call to [broadcast_updates] for a different
    RPC might broadcast your updates before your RPC is done running. As such, and because
    updates are published even if the RPC ultimately fails, it is necessary to first
    modify the feature, and only then call [on_*] so subscribers do not learn about the
    modification before it is ready.
*)

open! Core
open! Async
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val create
  : children_of:(Feature_path.t -> Feature.t list)
  -> unit
  -> t

val dump_subscriptions : t -> Sexp.t

(** Creating new subscriptions *)

val subscribe_feature_only
  :  t
  -> _ Query.t
  -> Feature_id.t
  -> when_to_first_notify:When_to_first_notify.t
  -> [ `Updated | `Archived ] Or_error.t Pipe.Reader.t

val subscribe_feature_and_descendants
  :  t
  -> _ Query.t
  -> Feature_path.t
  -> when_to_first_notify:When_to_first_notify.t
  -> Iron_protocol.Notify_on_descendant_updates.Reaction.t Or_error.t Pipe.Reader.t

(** Queueing new updates, and actually broadcasting them. *)

val on_archive : t -> Feature.t -> unit
val on_rename  : t -> Feature.t -> unit
val on_update  : t -> Feature.t -> unit

val broadcast_updates : t -> unit
