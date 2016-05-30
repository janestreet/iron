module Stable = struct

  open Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path            : Feature_path.V1.t
        ; feature_id              : Feature_id.V1.t option
        }
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
    (struct let name = "unarchive" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action                  = Stable.Action                  .Model
module Reaction                = Stable.Reaction                .Model
