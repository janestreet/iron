module Stable = struct

  open Import_stable

  module Action = struct
    module V2 = struct
      type t =
        { root_feature   : Feature_name.V1.t
        ; for_or_all     : [ `User of User_name.V1.t | `All_users ]
        ; include_active : bool
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = struct
      type t = Cr_soon_multiset.V1.t [@@deriving bin_io, sexp]

      let of_model t = t
    end
  end

end

include Iron_versioned_rpc.Make
    (struct let name = "get-cr-soons" end)
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V2
module Reaction = Stable.Reaction.V1
