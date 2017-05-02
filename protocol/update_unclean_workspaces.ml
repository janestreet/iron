module Stable = struct

  open! Import_stable

  module Action = struct
    module V3 = struct
      type t =
        { for_               : User_name.V1.t
        ; machine_name       : Machine_name.V1.t
        ; unclean_workspaces : Unclean_workspace.V2.t list
        ; clean_workspaces   : [ `Complement_of_those_listed_as_unclean
                               | `At_least_these of Feature_path.V1.t list
                               ]
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| c5e7e46bd753562a23d7530b2a1bcc49 |}]
      ;;

      let to_model (m : t) = m
    end

    module V2 = struct
      type t =
        { for_               : User_name.V1.t
        ; machine            : Machine_name.V1.t
        ; unclean_workspaces : Unclean_workspace.V2.t list
        ; clean_workspaces   : [ `Complement_of_those_listed_as_unclean
                               | `At_least_these of Feature_path.V1.t list
                               ]
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 1d860f5395df8d3447b23371b0f5fdfd |}]
      ;;

      open! Core
      open! Import

      let to_model { for_
                   ; machine
                   ; unclean_workspaces
                   ; clean_workspaces
                   } =
        V3.to_model
          { for_
          ; machine_name = machine
          ; unclean_workspaces
          ; clean_workspaces
          }
      ;;
    end

    module V1 = struct
      type t =
        { for_               : User_name.V1.t
        ; machine            : Machine_name.V1.t
        ; unclean_workspaces : Unclean_workspace.V1.t list
        ; clean_workspaces   : [ `Complement_of_those_listed_as_unclean
                               | `At_least_these of Feature_path.V1.t list
                               ]
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f2b57644ed1fac541aceb68307354c8c |}]
      ;;

      open! Core
      open! Import

      let to_model { for_
                   ; machine
                   ; unclean_workspaces
                   ; clean_workspaces
                   } =
        let unclean_workspaces =
          List.map unclean_workspaces ~f:Unclean_workspace.Stable.V1.to_v2
        in
        V2.to_model
          { for_
          ; machine
          ; unclean_workspaces
          ; clean_workspaces
          }
      ;;
    end

    module Model = V3
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "update-unclean-workspaces" end)
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
