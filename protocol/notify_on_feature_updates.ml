module Stable = struct

  open Import_stable

  module Action = struct
    module V2 = struct
      type t =
        { feature_id : Feature_id.V1.t
        ; when_to_first_notify : When_to_first_notify.V1.t
        }
      [@@deriving bin_io, sexp]

      let to_model (t : t) = t
    end

    module V1 = struct
      type t = Feature_id.V1.t
      [@@deriving bin_io]

      let to_model feature_id =
        V2.to_model
          { feature_id
          ; when_to_first_notify = At_next_change
          }
      ;;
    end

    module Model = V2
  end

  module Reaction = struct
    module V4 = struct
      type t = [ `Updated of Feature.Stable.V20.t
               | `Archived
               ]
      [@@deriving bin_io, sexp]

      let of_model t = t
    end

    module V3 = struct
      type t = [ `Updated of Feature.Stable.V19.t
               | `Archived
               ]
      [@@deriving bin_io]

      let of_model = function
        | `Updated feature -> `Updated (Feature.Stable.V19.of_model feature)
        | `Archived as t -> t
      ;;
    end

    module V2 = struct
      type t = [ `Updated of Feature.Stable.V18.t
               | `Archived
               ]
      [@@deriving bin_io]

      let of_model = function
        | `Updated feature -> `Updated (Feature.Stable.V18.of_model feature)
        | `Archived as t -> t
      ;;
    end

    module V1 = struct
      type t = [ `Updated of Feature.Stable.V17.t
               | `Archived
               ]
      [@@deriving bin_io]

      let of_model = function
        | `Updated feature -> `Updated (Feature.Stable.V17.of_model feature)
        | `Archived as t -> t
      ;;
    end

    module Model = V4
  end
end

include Iron_versioned_rpc.Make_pipe_rpc
    (struct let name = "notify-on-feature-updates" end)
    (struct let version = 4 end)
    (Stable.Action.V2)
    (Stable.Reaction.V4)

include Register_old_rpc
    (struct let version = 3 end)
    (Stable.Action.V2)
    (Stable.Reaction.V3)

include Register_old_rpc
    (struct let version = 2 end)
    (Stable.Action.V1)
    (Stable.Reaction.V2)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.Model
module Reaction = Stable.Reaction.Model
