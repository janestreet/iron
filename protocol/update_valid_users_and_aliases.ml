module Stable = struct

  open! Import_stable

  module User_aliases = struct
    module V1 = struct
      type t =
        { user_name           : User_name.V1.t
        ; aliases             : Alternate_name.V1.t list
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 1b38bb8f45e9213845af6e2c1e5daee7 |}]
      ;;
    end
    module Model = V1
  end

  module Action = struct
    module V2 = struct
      type t =
        { valid_users_and_aliases : User_aliases.V1.t list
        ; may_repartition_crs     : bool
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 08d793441b739e890c57e1416319d929 |}]
      ;;

      let to_model t = t
    end
    module Model = V2
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "update-valid-users-and-aliases" end)
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

module User_aliases = Stable.User_aliases. Model
module Action       = Stable.Action.       Model
module Reaction     = Stable.Reaction.     Model
