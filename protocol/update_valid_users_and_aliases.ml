module Stable = struct

  open Import_stable

  module User_aliases = struct
    module V1 = struct
      type t =
        { user_name           : User_name.V1.t
        ; aliases             : Alternate_name.V1.t list
        }
      [@@deriving bin_io, sexp]
    end
  end

  module Action = struct
    module V2 = struct
      type t =
        { valid_users_and_aliases : User_aliases.V1.t list
        ; may_repartition_crs     : bool
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
    (struct let name = "update-valid-users-and-aliases" end)
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

module User_aliases = Stable.User_aliases.V1
module Action       = Stable.Action.V2
module Reaction     = Stable.Reaction.V1
