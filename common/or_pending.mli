open! Core
open! Import

type 'a t =
  | Pending_since of Time.t
  | Known of 'a
[@@deriving bin_io, compare, sexp]

include Invariant.S1 with type 'a t := 'a t
include Monad.S      with type 'a t := 'a t
include Container.S1 with type 'a t := 'a t

val is_known   : _ t -> bool
val is_pending : _ t -> bool

val pending_now : unit -> _ t

val known_exn : 'a t -> 'a

val pending_error : 'a Or_error.t t -> 'a Or_error.t

val or_pending_error : 'a Or_error.t t -> ('a, Error.t t) Result.t

module Or_error : sig
  type nonrec 'a t = 'a Or_error.t t
  [@@deriving bin_io, compare, sexp]

  include Invariant.S1 with type 'a t := 'a t
  include Monad.S      with type 'a t := 'a t
  include Container.S1 with type 'a t := 'a t
end

module Stable : sig
  module V1 : Stable1 with type 'a t = 'a t
  module Or_error : sig
    module V1 : Stable1 with type 'a t = 'a Or_error.t
  end
end
