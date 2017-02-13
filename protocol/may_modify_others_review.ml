module Stable = struct

  open! Import_stable

  module Action = struct
    module V2 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; whose_review : User_name.Or_all_or_all_but.V1.t
        ; reason       : [ `Not_supported | `This of string ]
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 354c969d118a72d29b7121033b30aeb7 |}]
      ;;

      let to_model (t : t) = t
    end

    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; for_or_all   : User_name.Or_all.V1.t
        ; reason       : [ `Not_supported | `This of string ]
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 67d373a15f191ebdc49b112de8d38b7c |}]
      ;;

      let to_model { feature_path; for_or_all; reason; } =
        V2.to_model
          { feature_path
          ; whose_review = (for_or_all :> User_name.Or_all_or_all_but.V1.t)
          ; reason
          }
      ;;

    end
    module Model = V2
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "may-modify-others-review" end)
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
