open! Core
open! Async.Std
open! Import

val prune : root_feature : Feature_name.t -> Rev.t -> unit Deferred.t

val command : Command.t
