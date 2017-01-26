open! Core
open! Import

type t =
  | Whole_diff
  | Whole_diff_plus_ignored
  | For of User_name.t
  | None
[@@deriving sexp_of]

module Stable : sig
  module Model : T with type t = t
  module V2 : sig
    include Stable_without_comparator with type t = Model.t
    val to_model : t -> Model.t
  end
  module V1 : sig
    type t =
      | Whole_diff
      | Whole_diff_plus_ignored
      | For of User_name.Stable.V1.t
    include Stable_without_comparator with type t := t
    val to_model : t -> Model.t
    val to_v2    : t -> V2.t
  end
end
