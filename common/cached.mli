(** For caching an expensive computation and automatically recomputing when its
    dependencies change.

    One [create]s a cached value, supplying the function to compute, [compute_result], and
    a function, [compute_depends_on], that computes what [compute_result] depends on.
    Then, to get the value, one calls [get], which checks the dependencies and either
    returns the cached value or, if it is out of date, recomputes it using
    [compute_result].

    A value can be out of date either because what it depends on has changed, or, even if
    it has the same dependencies, because one of the dependencies has invalidated it.
    Client code is responsible for explicitly stating when a dependency invalidates the
    values that depend on it, by calling [Invalidator.invalidate_dependents].
*)

open! Core
open! Import

module Invalidator : sig
  type t [@@deriving sexp_of]

  include Equal.S     with type t := t
  include Invariant.S with type t := t

  val create : debug_information:Sexp.t -> t

  val invalidate_dependents : t -> unit

  (** Attach a call back to be run when [invalidate_dependents] is ticked.  Invariant: it
      is not intended for callback to raise any exceptions.  For robustness exceptions
      will be ignored in production.  To help debugging/tracking of patches breaking this
      invariant, any exception raised by a callback will make [invalidate_dependents]
      raise during functional tests. *)
  val set_callback : t -> callback_no_exn:(unit -> unit) -> unit
end

type 'a t [@@deriving sexp_of]
type packed = T : _ t -> packed

include Invariant.S1 with type 'a t := 'a t

module type S = sig
  type t [@@deriving sexp_of]
  include Equal.S with type t := t
end

(** If [compute_result] raises, then the error is cached and [get _] returns the error. *)
val create
  :  (module S with type t = 'a)
  -> compute_result     : (unit -> 'a)
  -> compute_depends_on : (unit -> Invalidator.t list)
  -> 'a t

val get : 'a t -> 'a Or_error.t

(** [clear] the cache, so that the next call to [get] calls [compute_result]. *)
val clear : _ t -> unit

(** Create a cached value that never needs to be recomputed, where [get] always returns
    [Error].  This is used to initialize cached values before they can be set to their
    actual value. *)
val uninitialized : name:string -> unit -> _ t

val check
  :  'a t
  -> ignore_diffs_in_errors : bool
  -> unit Or_error.t

(** Will raise if we are not in functional test or if there is an error while computing
    dependencies *)
val force_set_for_test_exn
  :  'a t
  -> 'a Or_error.t
  -> unit
