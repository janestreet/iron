module Stable = struct

  open Import_stable

  module Action = struct
    module V1 = Unit
  end

  module Reaction = struct
    module V1 = struct
      type t =
        { server_started_at : Time.V1_round_trippable.t
        }
      [@@deriving bin_io, fields, sexp]

      let of_model t = t
    end
  end

end

include Iron_versioned_rpc.Make
    (struct let name = "server-started-at" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V1
