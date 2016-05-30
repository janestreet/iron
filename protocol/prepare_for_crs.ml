module Stable = struct

  open Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
    end
  end

  module Reaction = struct
    module V2 = struct
      type t =
        { owner_for_crs   : User_name.V1.t
        ; alternate_names : User_name_by_alternate_name.V1.t
        ; aliases         : User_name_by_alternate_name.V1.t
        }
      [@@deriving bin_io, sexp]

      let of_model t = t
    end
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "prepare-for-crs" end)
    (struct let version = 2 end)
    (Stable.Action.V1)
    (Stable.Reaction.V2)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V2
