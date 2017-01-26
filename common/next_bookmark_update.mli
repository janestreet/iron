(** What Iron server thinks for a feature about the next bookmark update coming from Iron
    hydra. *)

open! Core
open! Import

type t =
  | Update_expected_since of Time.t
  | No_update_expected
  | No_update_expected_due_to_iron_bug of Error.t
[@@deriving compare, sexp_of]

val same_variant : t -> t -> bool

val am_expecting_bookmark_update : t -> bool

val to_or_error_or_pending : t -> unit Or_error.t Or_pending.t

val is_transition_to_update_expected : from:t -> to_:t -> bool

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
