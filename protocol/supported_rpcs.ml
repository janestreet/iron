module Stable = struct

  open Import_stable

  module Action = struct
    module V1 = Unit
  end

  module Reaction = struct
    module V1 = struct
      type t = Rpc_description.V1.t list
      [@@deriving bin_io, compare, sexp]

      let of_model m = m
    end
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "supported-rpcs" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   V1
module Reaction = Stable.Reaction. V1
