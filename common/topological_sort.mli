open! Core.Std
open! Import

module Edge : sig
  type 'a t =
    { from : 'a
    ; to_  : 'a
    }
  [@@deriving sexp_of]
end

(** [sort (module Node) nodes edges] returns a list of nodes [output] satisfying:

    - every node that appears in [nodes] or [edges] occurs once in [output].
    - if [{ from; to_ }] is in [edges], then [from] occurs before [to_] in [output].
    - nodes that have no incoming or outgoing edges appear sorted at the end of [output].

    [sort] returns [Error] if there is a cycle. *)
val sort
  :  (module Identifiable with type t = 'node)
  -> 'node list
  -> 'node Edge.t list
  -> 'node list Or_error.t
