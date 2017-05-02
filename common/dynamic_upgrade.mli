open! Core
open! Import

(** This module allows Iron admins to enable/disable new functionalities remotely while
    the server is running.  Once a new functionality is enabled, the [State.t] retains the
    first time it's actually used and from that point commits to that upgrade.  There is
    no going back to a lower upgrade once a functionality has been used. *)

type t =
  | U1
  | U2
  | U3
[@@deriving compare, sexp_of]

include Enum.S with type t := t

module State : sig

  type t [@@deriving sexp_of]

  include Invariant.S with type t := t

  module Reference : sig
    (** Invariant acting on references to the main [t] that's already checked as part of
        the server state. *)
    include Invariant.S with type t = t
  end

  val dump : t -> Sexp.t

  val deserializer : t Deserializer.t
end

(** Trying to go back to a lower upgrade number after some functionality associated with a
    higher upgrade has already happened will cause the function to raise.  On the other
    hand enabling a new upgrade always succeeds. *)
val set_exn : State.t -> t -> unit

val commit_to_upgrade
  : State.t
  -> allowed_from:t
  -> [ `Ok | `Not_allowed_yet ]

module Stable : sig
  module V1 : sig
    include Stable_without_comparator with type t = t
    val hash : t -> int
  end
end
