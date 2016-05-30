open! Core.Std
open! Async.Std

include Iron_common.Std
include Iron_hg.Std

module Hashtbl2_pair  = Core_extended.Std.Hashtbl2_pair
module Timed_event    = Iron_hg.Timed_event
module Which_features = Iron_protocol.Which_features
module Which_session  = Iron_protocol.Which_session

let (>>>) = `Deprecated_in_iron__Use_let_syntax_instead
let (>>=) = `Deprecated_in_iron__Use_let_syntax_instead
let (>>|) = `Deprecated_in_iron__Use_let_syntax_instead

let () = print_elapsed [%here]
