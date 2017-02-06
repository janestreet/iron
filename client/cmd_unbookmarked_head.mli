open! Core
open! Async
open! Import

val prune : root_feature : Feature_name.t -> Rev.t -> unit Deferred.t

val command : Command.t
