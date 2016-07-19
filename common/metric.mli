open! Core.Std
open! Import


module Stat_type : sig
  type t =
    | Count
    | Max
    | Min
    | Mean
    | Total
  [@@deriving compare, enumerate, sexp_of]

  module Compare_by_interest : sig
    type nonrec t = t [@@deriving compare, sexp_of]
  end

  include Comparable.S_plain with type t := t
end

module Snapshot : sig
  type t [@@deriving sexp_of]

  val aggregate : t -> t -> t

  val get_stat_as_string_hum : t -> Stat_type.t -> decimals:int -> string
end

(** A metric is a mutable type used to aggregate values of a time serie.  One can create a
    snapshot of the current state of a metric to send it to a client for display. *)
type t [@@deriving sexp_of]

include Invariant.S with type t := t

val create    : unit -> t
val add_value : t -> float -> unit
val snapshot  : t -> Snapshot.t

module Stable : sig
  module Snapshot : sig
    module Model : T with type t = Snapshot.t
    module V1 : sig
      type t = Model.t [@@deriving bin_io, sexp]
    end
  end
end
