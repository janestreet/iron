module Stable = struct
  open Import_stable

  module Action = struct
    module V3 = struct
      type t =
        { feature_path   : Feature_path.V1.t
        ; for_           : User_name.V1.t
        ; rev_zero       : Rev.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
    end

    module V2 = struct
      type t =
        { feature_path   : Feature_path.V1.t
        ; for_           : User_name.V1.t
        ; even_if_locked : bool
        ; rev_zero       : Rev.V1.t
        }
      [@@deriving bin_io]

      let to_model { feature_path
                   ; for_
                   ; even_if_locked = _
                   ; rev_zero
                   } =
        V3.to_model
          { V3.
            feature_path
          ; for_
          ; rev_zero
          }
      ;;
    end

    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; for_         : User_name.V1.t
        ; rev_zero     : Rev.V1.t
        }
      [@@deriving bin_io]

      let to_model { feature_path
                   ; for_
                   ; rev_zero
                   } =
        V3.to_model
          { V3.
            feature_path
          ; for_
          ; rev_zero
          }
      ;;
    end

    module Model = V3
  end

  module Reaction = struct
    module V1 = struct
      type t =
        { feature_tip      : Rev.V1.t
        ; parent_tip       : Rev.V1.t
        ; renames          : Rename.V1.t list
        ; remote_repo_path : Remote_repo_path.V1.t
        }
      [@@deriving bin_io, sexp]

      let of_model t = t
    end

    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "prepare-to-compress" end)
    (struct let version = 3 end)
    (Stable.Action.V3)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
