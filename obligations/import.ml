open! Core

include Iron_common.Std
include Int.Replace_polymorphic_compare

let (>>>) = `Deprecated_in_iron__Use_let_syntax_instead
let exit  = `Deprecated_in_iron__Use_shutdown_dot_exit_instead
let failwiths = `Deprecated_in_iron__Use_raise_s

let _ = Command.async

let () = print_elapsed [%here]
