open! Core
open! Import

module Feature : sig
  type t =
    { feature_path        : Feature_path.t
    ; include_descendants : bool
    }
  [@@deriving sexp_of]
end

type t =
  | All_features
  | Features of Feature.t list
[@@deriving sexp_of]

val these_features : Feature_path.t list -> t

val mem : t -> Feature_path.t -> bool

module Stable : sig
  module Feature : sig
    module V1 : Stable_without_comparator with type t = Feature.t
  end

  module V1 : sig
    include Stable_without_comparator with type t = t
    val to_model : t -> t
  end
end
