open! Core
open! Import

module What_to_do : sig
  type t =
    | Add    of Rev.t
    | Remove of Rev.t
  [@@deriving sexp_of]
end

module Action : sig
  type t =
    { what_to_do : What_to_do.t
    }
  [@@deriving fields, sexp_of]
end

module Reaction : Unit

include Iron_versioned_rpc.S
  with type action   = Action.t
  with type reaction = Reaction.t

module Stable : sig
  module What_to_do : sig
    module V1 : Stable_without_comparator with type t = What_to_do.t
  end
end
