module Stable = struct

  open Import_stable

  module Action = struct
    module V6 = struct
      type t =
        { feature_path           : Feature_path.V1.t
        ; for_or_all             : [ `All_users | `User of User_name.V1.t ]
        ; reason                 : string
        ; create_catch_up_for_me : bool
        ; base                   : Rev.V1.t option
        ; tip                    : Rev.V1.t option
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
    end

    module Model = V6

    module V5 = struct
      type t =
        { feature_path    : Feature_path.V1.t
        ; for_or_all      : [ `All_users | `User of User_name.V1.t ]
        ; reason          : string
        ; base            : Rev.V1.t option
        ; tip             : Rev.V1.t option
        }
      [@@deriving bin_io]

      let to_model
            { feature_path
            ; for_or_all
            ; reason
            ; base
            ; tip
            } =
        V6.to_model
          { V6.
            feature_path
          ; for_or_all
          ; reason
          ; create_catch_up_for_me = false
          ; base
          ; tip
          }
      ;;
    end

    module V4 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; for_or_all   : [ `All_users | `User of User_name.V1.t ]
        ; reason       : string
        }
      [@@deriving bin_io]

      let to_model { feature_path; for_or_all; reason } =
        V5.to_model
          { V5.
            feature_path
          ; for_or_all
          ; reason
          ; base         = None
          ; tip          = None
          }
      ;;
    end
  end

  module Reaction = struct
    module V1 = Unit
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "mark-fully-reviewed" end)
    (struct let version = 6 end)
    (Stable.Action.V6)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 5 end)
    (Stable.Action.V5)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 4 end)
    (Stable.Action.V4)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V6
module Reaction = Stable.Reaction.V1
