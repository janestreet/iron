open! Core
open! Async
open! Import

val maybe_send_push_event : repo_root:Repo_root.t -> unit Deferred.t

val command : Command.t
