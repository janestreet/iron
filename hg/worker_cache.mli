open! Core
open! Import

module From_server_to_worker : sig
  type t [@@deriving sexp_of]
  val empty : t

  module Concise : sig
    type nonrec t = t [@@deriving sexp_of]
  end
end

module From_worker_back_to_server : sig
  type t [@@deriving sexp_of]

  module Concise : sig
    type nonrec t = t [@@deriving sexp_of]
  end
end

module Worker_session : sig
  open! Async

  type t [@@deriving sexp_of]

  val create : From_server_to_worker.t -> t

  module Key : sig
    type 'a t =
      | Worker_obligations : Worker_obligations.t  t
      | Worker_rev_facts   : Worker_rev_facts.t    t
  end

  (** Return a deferred determined to a previous cached value, or call the closure
      provided and cache the result so that it will be part of the information sent back
      to the server when [back_to_server] is called.  There is no attempt in that module
      to deal with race condition in which [use_or_compute_and_store] is called multiple
      times on the same non cached revision before the closure provided terminates, or if
      [back_to_server] is called before those computation have finished computing. *)
  val use_or_compute_and_store :
    t
    -> 'a Key.t -> Rev.t -> (Rev.t -> 'a Deferred.t)
    -> 'a Deferred.t

  val find : t -> Rev.t -> 'a Key.t -> 'a option

  (** Used by the worker to return the set of values that were not available in the cache
      during the processing of the feature.  It will contain all the values returned by
      the closure provided to [use_or_compute_and_store] that have finished computing at
      the time [back_to_server] is called.  *)
  val back_to_server : t -> From_worker_back_to_server.t

  (** Used in some testing configuration to force the recomputation regardless of the
      state of the cache. *)
  val remove : t -> Rev.t -> _ Key.t -> unit
end

(** The cache on the server *)
type t [@@deriving sexp_of]

include Invariant.S with type t := t

module Status : sig
  type t =
    | Disabled
    | Write_only
    | Read_write
  [@@deriving enumerate, sexp_of]

  val doc_for_readme : unit -> string
end

val deserializer    : t Deserializer.t

val set_max_size          : t -> max_size:int          -> unit
val set_status            : t -> status:Status.t       -> unit
val set_max_items_per_rpc : t -> max_items_per_rpc:int -> unit

module What_to_dump : sig
  type t =
    [ `Stats
    | `Revs
    | `Values_at_rev      of Rev.t
    | `Obligations_at_rev of Rev.t
    ]
  [@@deriving sexp_of]

  val require_admin_privileges : t -> bool
end

val dump
  : t
  -> What_to_dump.t
  -> Sexp.t

module Feature_revs : sig
  type t =
    { diff4s_revs    : Rev.Compare_by_hash.Set.t
    ; base           : Rev.t
    ; tip            : Rev.t
    }
  [@@deriving sexp_of]
end

val clear
  : t
  -> [ `All
     | `Feature_revs of Feature_revs.t
     | `Revs of Rev.t list
     ]
  -> unit

val augment         : t -> From_worker_back_to_server.t -> unit
val send_to_worker  : t -> Feature_revs.t -> From_server_to_worker.t

module Stable : sig
  module From_server_to_worker : sig
    module V6 : sig
      include Stable_without_comparator with type t = From_server_to_worker.t
      val empty : t
    end
  end
  module From_worker_back_to_server : sig
    module V5 : sig
      include Stable_without_comparator with type t = From_worker_back_to_server.t
      val empty : t
    end
  end
  module Status : sig
    module V1 : Stable_without_comparator with type t = Status.t
  end
end
