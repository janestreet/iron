(** An expression denoting a set of users. *)

open! Core
open! Import

type t = User_name.Set.t [@@deriving sexp_of]

include Invariant.S with type t := t

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
