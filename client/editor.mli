open! Core
open! Async

(** Fire up a text editor to edit some string. *)
val invoke_editor
  :  ?tmpfile:string
  -> string
  -> string Or_error.t Deferred.t

(** Apply function [f] to the name of a freshly-created temporary file, delete the file
    after [f] returns.  If [?file] is of the form "<path>/<base>.<extension>" then the
    filename is of the form "<path>/<base><salt>.<extension>".  If no [?file] specified,
    the filename is of the form "tmp<salt>.tmp" in the current working directory.  *)
val with_temp_file
  :  ?file : string
  -> (string -> 'a Deferred.t)
  -> 'a Deferred.t
