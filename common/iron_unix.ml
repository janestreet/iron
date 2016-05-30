open! Core.Std
open! Async.Std
open! Import

include Unix

let exec = Core.Std.Unix.exec

let stdin_isatty  = Core.Std.Unix.(isatty stdin)
let stdout_isatty = Core.Std.Unix.(isatty stdout)

