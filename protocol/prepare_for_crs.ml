module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 82e9dd83ba682394b6a7532a1bdf9e67 |}]
      ;;

      let to_model t = t
    end
    module Model = V1
  end

  module Reaction = struct
    module V2 = struct
      type t =
        { owner_for_crs   : User_name.V1.t
        ; alternate_names : User_name_by_alternate_name.V1.t
        ; aliases         : User_name_by_alternate_name.V1.t
        }
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 9797eaf0ad4a52523cbb39251c6d114f |}]
      ;;

      let of_model t = t
    end
    module Model = V2
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "prepare-for-crs" end)
    (struct let version = 2 end)
    (Stable.Action.V1)
    (Stable.Reaction.V2)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
