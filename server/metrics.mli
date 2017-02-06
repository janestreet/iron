open! Core
open! Async
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val create : unit -> t

val complete : t -> prefix:string -> string list

val clear
  : t
  -> Iron_protocol.Metrics.Clear.Action.t
  -> Iron_protocol.Metrics.Clear.Reaction.t

val get
  : t
  -> Iron_protocol.Metrics.Get.Action.t
  -> Iron_protocol.Metrics.Get.Reaction.t

val add_values
  : t
  -> Iron_protocol.Metrics.Add_values.Action.t
  -> Iron_protocol.Metrics.Add_values.Reaction.t

(** Async subscription interface. *)

val dump_subscriptions : t -> Sexp.t

val subscribe :
  t
  -> _ Query.t
  -> Iron_protocol.Notify_on_metric_updates.Action.t
  -> Iron_protocol.Notify_on_metric_updates.Reaction.t Or_error.t Pipe.Reader.t
