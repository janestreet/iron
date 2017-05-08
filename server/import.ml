open! Core
open! Async

include Iron_common.Std
include Iron_hg.Std
include Int.Replace_polymorphic_compare

module Hashtbl2_pair  = Core_extended.Std.Hashtbl2_pair
module Timed_event    = Iron_hg.Timed_event

let (>>>) = `Deprecated_in_iron__Use_let_syntax_instead
let exit  = `Deprecated_in_iron__Use_shutdown_dot_exit_instead
let failwiths = `Deprecated_in_iron__Use_raise_s

let () = print_elapsed [%here]
