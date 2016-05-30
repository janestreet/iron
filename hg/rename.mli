open! Core.Std
open! Import

type t =
  { from : Feature_path.t
  ; to_  : Feature_path.t
  }
[@@deriving sexp_of]

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
