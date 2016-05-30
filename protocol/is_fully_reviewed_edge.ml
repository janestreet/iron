module Stable = struct

  open Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { from : Rev.V1.t
        ; to_  : Rev.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = Unit
  end

end

include Iron_versioned_rpc.Make
    (struct let name = "is-fully-reviewed-edge" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V1
