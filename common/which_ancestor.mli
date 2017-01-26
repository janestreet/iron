open! Core
open! Import

type t =
  | Any_root
  | Feature  of Feature_path.t
[@@deriving sexp_of]

module Stable : sig
  module Model : T with type t = t

  module V1 : sig
    type nonrec t = t =
      | Any_root
      | Feature  of Feature_path.Stable.V1.t
    include Stable_without_comparator with type t := t
    val of_model : Model.t -> t
    val to_model : t -> Model.t
  end
end
