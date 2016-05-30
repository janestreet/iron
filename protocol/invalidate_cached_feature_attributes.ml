module Stable = struct

  open Import_stable

  module Which_features = Which_features.Stable

  module Action = struct
    module V1 = Which_features.V1
  end

  module Reaction = struct
    module V1 = Unit
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "invalidate-cached-feature-attributes" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V1
