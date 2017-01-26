open! Core
open! Async.Std
open! Import

val set_description_exn : Feature_path.t -> string -> unit Deferred.t

val command : Command.t
