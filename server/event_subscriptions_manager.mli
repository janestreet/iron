open! Core
open! Async
open! Import

module Make (Key : sig
    type t [@@deriving sexp_of]
    include Hashable.S_plain with type t := t
  end) : sig
  type 'a t [@@deriving sexp_of]

  val invariant : _ t -> unit

  val dump   : _ t -> Sexp.t
  val create : unit -> _ t
  val tick   : 'a t -> Key.t -> 'a -> unit
  val add
    : 'a t -> _ Query.t -> Key.t -> initial:'a option -> 'a Or_error.t Pipe.Reader.t
  val close  : 'a t -> Key.t -> reason:'a -> unit
end
