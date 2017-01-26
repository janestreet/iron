open! Core
open! Import

module Reviewed_by_someone_else : sig
  type t =
    { reviewed_by : User_name.t
    ; reason      : string
    }
  [@@deriving compare, sexp_of]
end

type t =
  | Create_catch_up_for_me
  | Follower
  | Unfinished_review
  | Reviewed_by_someone_else of Reviewed_by_someone_else.t
[@@deriving compare, sexp_of]

include Invariant.S with type t := t

val to_string_hum : t -> string

module Stable : sig
  module Model : T with type t = t
  module V1 : sig
    include Stable_without_comparator with type t = Model.t
    val of_model : Model.t -> t
    val to_model : t -> Model.t
  end
end
