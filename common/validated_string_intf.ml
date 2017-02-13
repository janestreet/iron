open! Core
open! Import

module type Stable = sig
  type model
  type comparator_witness

  module V1 : sig
    include Stable_without_comparator with type t = model
    val hash : t -> int
    include Stringable.S with type t := t
    module Map : sig
      type 'a t = (model, 'a, comparator_witness) Map.t [@@deriving bin_io, compare, sexp]
      val hash : ('a -> int) -> 'a t -> int
    end
    module Set : sig
      type t = (model, comparator_witness) Set.t [@@deriving bin_io, compare, sexp]
      val hash : t -> int
    end
  end
end

module type Unstable = sig
  type t [@@deriving sexp_of]

  include Identifiable with type t := t
  include Invariant.S  with type t := t
end

module type S = sig
  include Unstable

  module Stable : Stable
    with type model := t
    with type comparator_witness := comparator_witness
end

module type Validated_string = sig

  module type S = S
  module type Stable = Stable
  module type Unstable = Unstable

  module Make (M : sig
      val module_name : string
      val check_valid : string -> unit Or_error.t
    end) () : S

  module Make_regex (M : sig
      val module_name : string
      val regex : Regex.t
    end) () : S
end
