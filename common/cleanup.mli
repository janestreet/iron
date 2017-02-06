(** For creating a cleanup action that one can explicitly run at any time, or will
    automatically be started when shutdown is called if it wasn't run explicitly.
    Shutdown will be delayed until the action finishes, whether the action was run
    explicitly or implicitly at shutdown. *)

open! Core
open! Async
open! Import

type t [@@deriving sexp_of]

val create : (unit -> unit Deferred.t) -> t

(** After calling [run t], subsequent calls to [run t] have no effect, and return the same
    deferred as the initial call. *)
val run : t -> unit Deferred.t

