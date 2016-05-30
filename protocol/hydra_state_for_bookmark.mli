open! Core.Std
open! Import

type t =
  { bookmark            : string
  ; first_12_of_rev     : Node_hash.First_12.t
  ; rev_author_or_error : User_name.t Or_error.t
  ; status              : [ `Done | `Pending_or_working_on_it ]
  }
[@@deriving sexp_of]

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
