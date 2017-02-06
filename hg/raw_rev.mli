open! Core
open! Async
open! Import
open Hg

type t =
  | Rev    of Rev.t
  | String of string
[@@deriving sexp_of, variants]

val resolve
  :  t
  -> in_ : Repo_root.t Or_error.t
  -> Rev.t Or_error.t Deferred.t

val resolve_exn
  :  t
  -> in_ : Repo_root.t Or_error.t
  -> Rev.t Deferred.t

val resolve_opt_exn
  :  t option
  -> in_ : Repo_root.t Or_error.t
  -> Rev.t option Deferred.t

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
