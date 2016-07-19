module Stable = struct

  open Import_stable

  module Feature = Feature.Stable

  module Action = struct
    module V1 = struct
      type t = Feature_path.V1.t
      [@@deriving bin_io, sexp]

      let to_model t = t
    end

    module Model = V1
  end

  module Reaction = struct
    module V2 = struct
      type t = No | Yes of Feature_id.V1.t
      [@@deriving bin_io, sexp]

      let of_model (t : t) = t
    end

    module V1 = struct
      type t = bool
      [@@deriving bin_io]

      let of_model m =
        match V2.of_model m with
        | No -> false
        | Yes (_ : Feature_id.V1.t) -> true
      ;;
    end

    module Model = V2
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "feature-exists" end)
    (struct let version = 2 end)
    (Stable.Action.V1)
    (Stable.Reaction.V2)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model

