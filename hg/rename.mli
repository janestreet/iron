open! Core
open! Import

type t =
  { feature_id : Feature_id.t
  ; from       : Feature_path.t
  ; to_        : Feature_path.t
  }
[@@deriving sexp_of]

module Stable : sig
  module V2 : Stable_without_comparator with type t = t
  module V1 : sig
    type t [@@deriving bin_io]
    val of_v2 : V2.t -> t
  end
end
