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
    module V1 = struct
      type t =
        { de_aliased                                : User_name.V1.Set.t
        ; did_not_de_alias_due_to_non_empty_session : User_name.V1.Set.t
        ; nothing_to_do                             : User_name.V1.Set.t
        }
      [@@deriving bin_io, compare, sexp]

      let of_model t = t
    end
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "de-alias-feature" end)
    (struct let version = 1 end)
    (Stable.Action  .V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action  .V1
module Reaction = Stable.Reaction.V1
