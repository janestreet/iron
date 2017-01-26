open! Core
open! Import

module Push_event : sig
  type t [@@deriving sexp_of]

  val at           : t -> Time.t
  val by           : t -> User_name.t
  val feature_path : t -> Feature_path.t

  val mark_as_used_by_metrics : t -> Metric_name.t -> [ `Already_marked
                                                      | `Ok
                                                      ]
end

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val add
  : t
  -> Iron_protocol.Push_events.Add.Action.t Query.t
  -> feature_path:Feature_path.t
  -> Iron_protocol.Push_events.Add.Reaction.t

val change
  : t
  -> Iron_protocol.Push_events.Change.Action.t
  -> Iron_protocol.Push_events.Change.Reaction.t

val find
  : t
  -> Node_hash.First_12.t
  -> feature_id:Feature_id.t
  -> Push_event.t option

val deserializer : t Deserializer.t

val dump
  : t
  -> Iron_protocol.Push_events.What_to_dump.t
  -> Sexp.t
