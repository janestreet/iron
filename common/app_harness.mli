open! Core
open! Async.Std
open! Import

val start :
  init_stds:bool
  -> log_format:Log.Output.format
  -> main:(basedir:string -> unit Deferred.t)
  -> basedir:string
  -> mode:[ `Dev | `Prod ]
  -> fg: bool
  -> unit
  -> unit

val commands
  :  appname:string
  -> appdir_for_doc:string
  -> appdir:string
  -> log_format:Log.Output.format
  -> start_spec:('a, basedir:string -> unit Deferred.t) Command.Spec.t
  -> start_main:'a
  (* A list of start/stop/status commands *)
  -> (string * Command.t) list
