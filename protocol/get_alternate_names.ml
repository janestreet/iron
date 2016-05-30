module Stable = struct
  open Import_stable

  module Action = struct
    module V1 = struct
      type t =
        [ `Aliases
        | `Typos
        | `All
        ]
      [@@deriving bin_io, compare, sexp]

      let to_model t = t
    end

    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t = User_name_by_alternate_name.V1.t
      [@@deriving bin_io, compare, sexp]

      let of_model t = t
    end

    module Model = V1
  end
end

open! Core.Std
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "get-alternate-names" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.  Model
module Reaction = Stable.Reaction.Model
