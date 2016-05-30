open! Core.Std
open! Async.Std

include module type of struct include Async.Std.Command end

val async'
   : summary:string
  -> ?readme:(unit -> string)
  -> (unit -> unit Deferred.t) Spec.param
  -> t
val async          : [ `Use_async'_instead ]
val async_basic    : [ `Async_basic_is_deprecated ]
val async_or_error : [ `Redefine_async_or_error_in_iron_command_if_needed ]
