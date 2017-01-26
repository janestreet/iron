open! Core
open! Import

type +'a t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

val create : ?by:User_name.t -> ?at:Time.t -> 'a -> 'a t

(** Accessors. *)
val action : 'a t -> 'a
val at     : _ t -> Time.t
val by     : _ t -> User_name.t
val uuid   : _ t -> Uuid.t

val with_action : 'a t -> 'b -> 'b t

module Stable : sig
  module V1 : Stable1 with type 'a t = 'a t
end
