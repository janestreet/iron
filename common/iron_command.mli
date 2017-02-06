open! Core
open! Async

include module type of struct include Async.Command end

val async'
  :  summary:string
  -> ?readme:(unit -> string)
  -> (unit -> unit Deferred.t) Spec.param
  -> t
val async          : [ `Use_async'_instead ]
val async_basic    : [ `Async_basic_is_deprecated ]
val async_or_error : [ `Use_async'_instead ]
val async_or_error' : [ `Use_async'_instead ]
