module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = Unit
  end

  module Reaction = struct
    module V1 = Unit
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "check-invariant" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V1
