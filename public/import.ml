open! Core

(* Stop : do not include Iron_common, Iron_hg, Iron_protocol.  We use [Export] to
   introduce the aliases that we need to deal with Iron modules *)
include Export
include Int.Replace_polymorphic_compare

module type Unit = sig
  type t = unit
  [@@deriving sexp_of]
end

let print_elapsed         = Iron_common.Std.print_elapsed
let am_functional_testing = Iron_common.Std.am_functional_testing

module Verbose = Iron_common.Std.Verbose

let (>>>) = `Deprecated_in_iron__Use_let_syntax_instead
let (>>=) = `Deprecated_in_iron__Use_let_syntax_instead
let (>>|) = `Deprecated_in_iron__Use_let_syntax_instead
let exit  = `Deprecated_in_iron__Use_shutdown_dot_exit_instead
let failwiths = `Deprecated_in_iron__Use_raise_s

let () = print_elapsed [%here]
