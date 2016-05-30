module Stable = struct

  open Import_stable

  module Which_features = Which_features.Stable

  module Action = struct
    module V1 = struct
      type t =
        { which_features         : Which_features.V1.t
        ; ignore_diffs_in_errors : bool
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
    (struct let name = "check-cached-feature-attributes" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.  V1
module Reaction = Stable.Reaction.V1
