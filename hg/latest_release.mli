open! Core
open! Import

type t =
  { released_feature      : Released_feature.t
  ; diff_from_base_to_tip : Diff2s.t
  }
[@@deriving fields, sexp_of]

include Invariant.S  with type t := t

module Stable : sig
  module Model : T with type t = t
  module V1 : sig
    type t = Model.t
    include Stable_without_comparator with type t := t
    val of_model : Model.t -> t
    val to_model : t -> Model.t
  end
end
