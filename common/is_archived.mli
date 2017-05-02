open! Core
open! Import

type t =
  | Yes of { reason_for_archiving : string }
  | No
[@@deriving sexp_of]

val to_bool : t -> bool

module Stable : sig
  module Model : T with type t = t

  module V1 : sig
    include Stable_without_comparator with type t = Model.t

    val to_bool : t -> bool
  end
end
