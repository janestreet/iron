open Core
open! Import

include Incremental_lib.Incremental.Make ()

let should_stabilize = ref true

let stabilize () = if !should_stabilize then stabilize ()

let set_should_stabilize bool =
  if not (Bool.equal !should_stabilize bool)
  then (
    should_stabilize := bool;
    if !should_stabilize then stabilize ())
;;
