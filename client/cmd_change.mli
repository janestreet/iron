open! Core
open! Async
open! Import

val command : Command.t

val change_feature
  :  ?verbose     : bool
  -> feature_path : Feature_path.t
  -> updates      : Change_feature.Update.t list
  -> unit
  -> unit Deferred.t

val apply_updates_exn
  :  sexp_of_update    : ('update -> Sexp.t)
  -> rpc_to_server_exn : (feature_path : Feature_path.t
                          -> updates   : 'update list
                          -> ('update * unit Or_error.t) list Deferred.t)
  -> ?verbose          : bool
  -> feature_path      : Feature_path.t
  -> updates           : 'update list
  -> unit
  -> unit Deferred.t
