module Stable = struct

  open Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { from_                  : Feature_path.V1.t
        ; to_                    : Feature_path.V1.t
        ; rev_zero               : Rev.V1.t
        ; without_copying_review : bool
        }
      [@@deriving bin_io, fields, sexp]

      let to_model t = t
    end

    module Model = V1
  end

  module Reaction = struct
    module V2 = struct
      type t =
        { feature_id       : Feature_id.V1.t
        ; remote_repo_path : Remote_repo_path.V1.t
        ; tip              : Rev.V1.t
        }
      [@@deriving bin_io, sexp]

      let of_model (t : t) = t
    end

    module V1 = struct
      type t =
        { remote_repo_path : Remote_repo_path.V1.t
        ; tip              : Rev.V1.t
        }
      [@@deriving bin_io]

      let of_model m =
        let { V2.
              feature_id = _
            ; remote_repo_path
            ; tip
            } = V2.of_model m in
        { remote_repo_path
        ; tip
        }
      ;;
    end

    module Model = V2
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "copy-feature" end)
    (struct let version = 2 end)
    (Stable.Action.V1)
    (Stable.Reaction.V2)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
