open! Core
open! Import

module type S = sig

  type t [@@deriving sexp]

  include Identifiable with type t := t
  include Invariant.S  with type t := t

  module Stable : sig
    module V1 : sig
      include Stable_without_comparator with type t = t
      val hash : t -> int
      include Stringable.S with type t := t
      module Map : sig
        type 'a t = 'a Map.t [@@deriving sexp, bin_io, compare]
        val hash : ('a -> int) -> 'a t -> int
      end
      module Set : sig
        type t = Set.t [@@deriving sexp, bin_io, compare]
        val hash : t -> int
      end
    end
  end
end

module type Validated_string = sig

  module type S = S

  module Make (M : sig
      val module_name : string
      val check_valid : string -> unit Or_error.t
    end) () : S

  module Make_regex (M : sig
      val module_name : string
      val regex : Regex.t
    end) () : S
end
