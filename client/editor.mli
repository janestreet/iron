open! Core
open! Async

(* Fire up a text editor to edit some string. *)
val invoke_editor
  :  ?tmpfile:string
  -> string
  -> string Or_error.t Deferred.t

val with_temp_file
  :  ?file : string
  -> (string -> 'a Deferred.t)
  -> 'a Deferred.t
