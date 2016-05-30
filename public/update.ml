module Stable = struct

  open Import_stable

  module Action = struct
    module V1 = struct
      type t =
        | Satellites_only
        | Feature of Feature_path.V1.t
      [@@deriving bin_io, sexp]

      let to_model m = m
    end

    module Model = V1
  end

  module Reaction = struct
    module V1 = Unit

    module Model = V1
  end
end

include Iron_command_rpc.Make
    (struct let name = "update" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action   .Model
module Reaction = Stable.Reaction .Model
