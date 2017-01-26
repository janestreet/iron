open! Core
open! Import

(** This type represents the result of a computation that can partially fail.  In that
    case, depending on what we are trying to do, we want either the whole line count and
    fail if it cannot be computed, or get as much of the line count as we can compute. *)

module Partially_known : sig
  type 'a t =
    { to_finish_session        : 'a
    ; from_session_end_to_goal : Error.t Or_pending.t
    }
end

type 'a t =
  | Fully_known     of 'a
  | Partially_known of 'a Partially_known.t
[@@deriving compare, sexp_of]

include Invariant.S1 with type 'a t := 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val fully_known_exn       : 'a t -> 'a
val maybe_partially_known : 'a t -> 'a
