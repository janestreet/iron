open! Core
open! Async.Std
open! Import

include Unix

let exec = Core.Unix.exec

let stdin_isatty  = Core.Unix.(isatty stdin)
let stdout_isatty = Core.Unix.(isatty stdout)

