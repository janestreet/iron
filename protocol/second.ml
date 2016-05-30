module Stable = struct

  open Import_stable

  module Action = struct
    module V4 = struct
      type t =
        { feature_path      : Feature_path.V1.t
        ; even_though_empty : bool
        ; even_though_owner : bool
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
    end

    module Model = V4

    module V3 = struct
      type t =
        { feature_path      : Feature_path.V1.t
        ; for_              : User_name.V1.t
        ; even_though_empty : bool
        ; even_though_owner : bool
        }
      [@@deriving bin_io]

      let to_model { feature_path; even_though_empty; even_though_owner; _ } =
        { Model. feature_path; even_though_empty; even_though_owner }
      ;;
    end
  end

  module Reaction = struct
    module V1 = Unit
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "second" end)
    (struct let version = 4 end)
    (Stable.Action.V4)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 3 end)
    (Stable.Action.V3)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V4
module Reaction = Stable.Reaction.V1
