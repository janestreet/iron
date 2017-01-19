module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { for_               : User_name.V1.t
        ; machine            : Machine.V1.t
        ; unclean_workspaces : Unclean_workspace.V1.t list
        ; clean_workspaces   : [ `Complement_of_those_listed_as_unclean
                               | `At_least_these of Feature_path.V1.t list
                               ]
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f2b57644ed1fac541aceb68307354c8c |}]
      ;;

      let to_model m = m
    end

    module Model = V1
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "update-unclean-workspaces" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
