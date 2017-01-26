open! Core
open! Import

type t =
  | Num of int
  | Commit
[@@deriving compare, sexp_of]

include Invariant.S with type t := t

(** [count Commit] = 1, [count (Num n) = n] *)
val count : t -> int

val to_string_hum : t -> string

module Stable : sig
  module Model : T with type t = t
  module V1 : sig
    include Stable_without_comparator with type t = Model.t
    val of_model : Model.t -> t
    val to_model : t -> Model.t
  end
end
