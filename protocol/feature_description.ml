module Stable = struct

  open! Import_stable

  module Action = struct
    module V4 = struct
      include Maybe_archived_feature_spec.V3

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 4164209ba45744542ce9faf3844df40e |}]
      ;;

      let to_model t = t
    end

    module V3 = struct
      include Maybe_archived_feature_spec.V2

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| bad769765b762926f3053429c38bf52b |}]
      ;;

      let to_model t = V4.to_model (Maybe_archived_feature_spec.V2.to_v3 t)
    end

    module V2 = struct
      include Maybe_archived_feature_spec.V1

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| cf83d33ba9da1caac7ec0a230194119c |}]
      ;;

      let to_model t = V3.to_model (Maybe_archived_feature_spec.V1.to_v2 t)
    end

    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 82e9dd83ba682394b6a7532a1bdf9e67 |}]
      ;;

      let to_model { feature_path } =
        V2.to_model (V2.existing_feature_path feature_path)
      ;;
    end

    module Model = V4
  end

  module Reaction = struct
    module V1 = struct
      type t =
        { description : string
        }
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 5b4a7ae4f8de1da8826df676ece43bc8 |}]
      ;;

      let of_model t = t
    end

    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "feature-description" end)
    (struct let version = 4 end)
    (Stable.Action.V4)
    (Stable.Reaction.V1)

include Register_old_rpc
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

module Action   = Stable.Action.  Model
module Reaction = Stable.Reaction.Model
