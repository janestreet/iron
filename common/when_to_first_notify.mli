(** This module represents whether the various notify-* pipe rpcs should provide an
    initial value in the pipe or not.
    In general, having an initial value with [Now] is desirable for programs when these
    rpcs are used as an alternative to polling (otherwise you must be sure to check the
    state of the feature after you receive the pipe but before you process any of its
    updates to avoid missing updates due to race conditions).
    When these rpcs are used for debugging or human consumption, then it's usually
    preferable to not have a spurious event, and [At_next_change] can be used.
*)

open! Core
open! Import

type t =
  | Now
  | At_next_change
[@@deriving sexp_of]

include Enum.S     with type t := t
include Comparable with type t := t

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
