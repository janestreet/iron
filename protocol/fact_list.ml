module Stable = struct

  open Import_stable

  module Action = struct
    module V1 = struct
      type t = Fact.Spec.Id.V1.t [@@deriving bin_io, sexp]
      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = struct
      type t = (Fact.Scope.V1.t * Fact.Evidence.V1.t) list [@@deriving bin_io, sexp]
      let of_model t = t
    end
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "fact-list" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V1
