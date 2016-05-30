open! Core.Std.No_polymorphic_compare

module Stable = struct

  open Import_stable

  module V1 = struct
    type t =
      { bookmark            : string
      ; first_12_of_rev     : Node_hash.First_12.V1.t
      ; rev_author_or_error : User_name.V1.t Or_error.V1.t
      ; status              : [ `Done | `Pending_or_working_on_it ]
      }
    [@@deriving bin_io, compare, sexp]
  end
end

include Stable.V1
