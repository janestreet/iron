open! Core.Std;;

(* A polymorphic list-hash function. *)
val hash : ('a -> int) -> 'a list -> int
