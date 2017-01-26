open Core
(* No [open Import].  This module is used in import.ml. *)

module type Stable_identifiable = sig
  include Identifiable
  val bin_read_t  : [ `not_stable ]
  val bin_write_t : [ `not_stable ]
  val bin_size_t  : [ `not_stable ]
  val t_of_sexp   : [ `not_stable ]
  module Stable : sig
    module V1 : sig
      type nonrec t = t
      type nonrec comparator_witness = comparator_witness
      include Stable
        with type t := t
        with type comparator_witness := comparator_witness
      include Comparable.Stable.V1.S
        with type comparable := t
        with type comparator_witness := comparator_witness
    end
  end
end

module Stable_identifiable_string = struct
  include String
  let bin_read_t  = `not_stable
  let bin_write_t = `not_stable
  let bin_size_t  = `not_stable
  let t_of_sexp   = `not_stable
end
