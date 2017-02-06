open! Core
open! Async
open! Import

(** Or one level lower for vim users or people running jenga via the terminal  *)
module Jenga : sig
  type t
  val find_in_enclosing_repo_root : Repo_root.t -> t option Deferred.t
  val kill : t -> unit Or_error.t Deferred.t
end

module Project_id : sig
  type t

  val of_jenga : Jenga.t -> t
end

val kill_project : Project_id.t -> string Or_error.t Deferred.t
