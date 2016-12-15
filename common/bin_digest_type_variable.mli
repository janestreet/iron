(** Bin_shape does not allow directly hashing type constructors (e.g., ['a t]). The type
    parameters must be instantiated to concrete types. This module defines uninhabited
    types reserved for instantiating these parameters. They are guaranteed not to collide
    with types actually used in other code. *)

type tick_a [@@deriving bin_io]

module Stable : sig
  type nonrec tick_a = tick_a [@@deriving bin_io]
end
