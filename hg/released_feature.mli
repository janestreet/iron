open! Core
open! Import

type t =
  { feature_id              : Feature_id.t
  ; feature_path            : Feature_path.t
  ; description             : string
  ; owners                  : User_name.t list
  ; whole_feature_followers : User_name.Set.t
  ; whole_feature_reviewers : User_name.Set.t
  ; seconder                : User_name.t
  ; base                    : Rev.t
  ; tip                     : Rev.t
  ; properties              : Properties.t
  ; includes                : t list
  ; release_cause           : unit Query.t
  }
[@@deriving fields, sexp_of]

include Invariant.S  with type t := t

val released_at : t -> Time.t
val released_by : t -> User_name.t

val attribute_table : t -> Ascii_table.t

module Stable : sig
  module Model : T with type t = t
  module V3 : sig
    type t = Model.t
    include Stable_without_comparator with type t := t
    val of_model : Model.t -> t
    val to_model : t -> Model.t
  end
  module V2 : sig
    include Stable_without_comparator
    val of_model : Model.t -> t
    val to_model : t -> Model.t
    val of_v3 : V3.t -> t
  end
  module V1 : sig
    include Stable_without_comparator
    val of_model : Model.t -> t
    val to_model : t -> Model.t
  end
end
