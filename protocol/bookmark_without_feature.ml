module Stable = struct

  open Import_stable

  module V1 = struct
    type t =
      { bookmark        : string
      ; first_12_of_rev : Node_hash.First_12.V1.t
      }
    [@@deriving bin_io, compare, fields, sexp]
  end
end

include Stable.V1
