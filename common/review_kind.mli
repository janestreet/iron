open! Core
open! Import

(** [Must_review] review that must be done for the feature to be releasable. This kind of
    review can become follow review depending on what others have read, or other
    conditions (like changes being reverted).

    [Follow] review that doesn't block release.

    [May_review] review for diff4s that you could read, but have already been read by
    sufficiently many other people that you are not needed anymore. *)
type t =
  | Must_review
  | Follow
  | May_review
  | Ownership_change
[@@deriving compare, sexp_of]


include Invariant.S with type t := t
include Equal.S     with type t := t

val to_string_hum : t -> string

module Stable : sig
  module Model : T with type t = t
  module V1 : sig
    include Stable_without_comparator with type t = Model.t
    val of_model : Model.t -> t
    val to_model : t -> Model.t
  end
end
