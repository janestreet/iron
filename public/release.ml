module Stable = struct

  open Import_stable

  module Included_features_order = struct
    module V1 = struct
      type t =
        [ `Name
        | `Release_order
        ]
      [@@deriving bin_io, sexp]
    end

    module Model = V1
  end

  module Action = struct
    module V1 = struct
      type t =
        { feature_path            : Feature_path.V1.t
        ; for_                    : User_name.V1.t
        ; included_features_order : Included_features_order.V1.t
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
    (struct let name = "release" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action                  = Stable.Action                  .Model
module Included_features_order = Stable.Included_features_order .Model
module Reaction                = Stable.Reaction                .Model
