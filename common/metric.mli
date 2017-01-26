open! Core
open! Import


module Data_point : sig
  type t =
    { at    : Time.t
    ; value : float
    }
  [@@deriving fields, sexp_of]
end

(** A metric is a mutable type used to aggregate values of a time serie.  One can get the
    state of a metric as a list of data points to send it to a client for display. *)
type t [@@deriving sexp_of]

include Invariant.S with type t := t

val create      : unit -> t
val add         : t -> Data_point.t -> unit
val data_points : t -> Data_point.t list

module Stable : sig
  module Data_point : sig
    module Model : T with type t = Data_point.t
    module V1 : sig
      type t = Model.t [@@deriving bin_io, sexp]
    end
  end
end
