open! Core
open! Import

(** This module is part of the runtime of fe server.  It is in hg/ just because some other
    part of the runtime is there too (such as review_sessions).  This module shall not be
    used by client code linking with Iron_hg, nor is it exposed in [Iron_hg.Std]. *)

module Id : Iron_common.Iron_uuid_intf.S

module Action : sig
  module Next_base_update_expiration : sig
    type t =
      { feature_id    : Feature_id.t
      }
    [@@deriving compare, sexp_of]
  end

  type t =
    | Next_base_update_expiration of Next_base_update_expiration.t
  [@@deriving compare, sexp_of]
end

type t
[@@deriving sexp_of]

include Invariant.S with type t := t

val run
  : [ `After of Time.Span.t
    | `At    of Time.t
    ]
  -> Action.t
  -> t

val abort_if_possible : t -> unit

(** This should be used when an event is executing, to check whether the event is still
    applicable (ie, to prevent race conditions). *)
val has_id : t -> Id.t -> bool

val scheduled_at : t -> Time.t

val set_execute_exn : execute:(Id.t -> Action.t -> unit) -> unit

module Table : sig
  type t
  [@@deriving sexp_of]

  include Invariant.S with type t := t

  val dump : t -> Sexp.t

  module Errors : sig
    val get   : t -> Error.t list
    val clear : t -> unit
  end
end

val the_table : unit -> Table.t
