module Stable = struct

  open Import_stable

  module Action = struct
    module V1 = struct

      type t =
        { feature_path                : Feature_path.V1.t
        ; base                        : Raw_rev.V1.t option
        ; tip                         : Raw_rev.V1.t option
        ; description                 : string option
        ; owners                      : User_name.V1.t list
        ; is_permanent                : bool
        ; remote_repo_path            : Remote_repo_path.V1.t option
        ; no_bookmark                 : bool
        ; add_whole_feature_reviewers : User_name.V1.Set.t option
        ; allow_non_cr_clean_base     : bool
        ; properties                  : Properties.V1.t option
        }
      [@@deriving bin_io, fields, sexp]

      let to_model m = m
    end

    module Model = V1
  end

    module Reaction = struct
    module V1 = Unit

    module Model = V1
  end
end

include Iron_command_rpc.Make
    (struct let name = "create" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action   .Model
module Reaction = Stable.Reaction .Model
