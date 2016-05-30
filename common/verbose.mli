open! Core.Std
open! Import

include module type of struct include Iron_options.Verbose end

val message : string -> 'a -> ('a -> Sexp.t) -> unit
