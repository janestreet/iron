(** [Event_subscriptions] keeps track of the limits on pipe rpcs, and which ones are
    currently open, for debugging and administration. The actual pipes are handled
    by [Feature_updates_manager] instead. *)

open! Core
open! Async
open! Import

module Subscription : sig
  type t

  (** [closed t] returns a deferred that gets determined when affected by either of
      [drop_all_by_user] or [remove], with a variant indicating which was
      it.  A subscription that gets dropped still needs to be removed. *)
  val closed : t -> [ `Removed
                    | `Dropped of Error.t
                    ] Deferred.t

  val tick : t -> unit
end

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val add
  :  t
  -> rpc_name:string
  -> rpc_version:int
  -> 'a Query.t
  -> sexp_of_action:('a -> Sexp.t)
  -> Subscription.t Or_error.t

val remove : t -> Subscription.t -> unit

val deserializer : t Deserializer.t

val set_max_subscriptions_per_user : t -> int -> unit

val set_max_subscriptions_global : t -> int -> unit

val drop_all_by_user
  :  t
  -> Iron_protocol.With_event_subscriptions.Action.t Query.t
  -> User_name.Or_all.t
  -> unit

val dump : t -> Sexp.t
