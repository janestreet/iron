open! Core
open! Import

(** A hydra repo controller is a pair of a repository and an action to do when the repo
    changes. *)

type t [@@deriving sexp_of]

include Invariant.S        with type t := t
include Stringable.S       with type t := t
include Hashable.S_plain   with type t := t
include Comparable.S_plain with type t := t

val submissions        : Feature_path.t -> t
val continuous_release : Feature_path.t -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t
    include Stable_without_comparator
      with type t := t
    include Comparable.Stable.V1.S
      with type comparable := t
      with type comparator_witness = comparator_witness
  end
end
