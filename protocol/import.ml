open! Core

include Iron_common.Std
include Iron_obligations.Std
include Iron_hg.Std
include Int.Replace_polymorphic_compare

module type Unit = sig
  type t = unit
  [@@deriving sexp_of]
end

let (>>>) = `Deprecated_in_iron__Use_let_syntax_instead
let exit  = `Deprecated_in_iron__Use_shutdown_dot_exit_instead

let () = print_elapsed [%here]
