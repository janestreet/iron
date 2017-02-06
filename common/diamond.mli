open! Core
open! Async
open! Import

module Node : sig
  type t = [ `old_base | `new_base | `old_tip | `new_tip ]
  [@@deriving compare, sexp_of]
  val to_string_short : t -> string
end

type 'a t =
  { b1 : 'a
  ; f1 : 'a
  ; b2 : 'a
  ; f2 : 'a
  }
[@@deriving compare, fields, sexp]

val old_base : 'a t -> 'a
val new_base : 'a t -> 'a
val old_tip  : 'a t -> 'a
val new_tip  : 'a t -> 'a

include Container.S1 with type 'a t := 'a t
include Applicative  with type 'a t := 'a t

val for_all2 : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool

val edges : 'a t -> ('a * 'a) list

val singleton : 'a -> 'a t
val of_one_edge : 'a -> 'a -> 'a t

val get  : 'a t -> Node.t -> 'a
val get' : 'a t -> [ `b1 | `b2 | `f1 | `f2 ] -> 'a

val init : f:(Node.t -> 'a) -> 'a t
val self : Node.t t

module Deferred : sig
  val map
    :  ?how:[ `Sequential | `Parallel ]
    -> 'a t
    -> f:('a -> 'b Deferred.t)
    -> 'b t Deferred.t
  val map2
    :  ?how:[ `Sequential | `Parallel ]
    -> 'a t
    -> 'b t
    -> f:('a -> 'b -> 'c Deferred.t)
    -> 'c t Deferred.t
  val all
    : 'a Deferred.t t -> 'a t Deferred.t
end

val param
  : 'a Command.Flag.t
  -> doc:(string -> string)
  -> 'a t Command.Param.t

val anon : (Node.t -> 'a Command.Spec.anons) -> 'a t Command.Param.t

val pretty_short_description     : label:string -> string t -> (string * string) list
val pretty_short_rev_names       : equal:('a -> 'a -> bool) -> 'a t -> string t
val pretty_short_rev_names_const : string t

(** [group t ~by] returns a new diamond where each point has all points in [t] equivalent
    to it according to [by]. *)
val group : 'a t -> by:Diff4_class.t -> 'a list t

val is_forget
  :  equal:('a -> 'a -> bool)
  -> 'a t
  -> bool

(** Detect the equivalence cases depending on the equality that hold between the 4 nodes
    of a revision graph *)
val classify
  :  equal:('a -> 'a -> bool)
  -> 'a t
  -> Diff4_class.t

include Invariant.S1 with type 'a t := 'a t
module Stable : sig
  module V1 : Stable1 with type 'a t = 'a t
end
