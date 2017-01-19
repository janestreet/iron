module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; rev_zero     : Rev.V1.t
        ; feature_id   : Feature_id.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 3e689ec54ac419feb5c9c6e3f71caafb |}]
      ;;

      let to_model t = t
    end
    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t =
        { feature_tip      : Rev.V1.t
        ; remote_repo_path : Remote_repo_path.V1.t
        }
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| c359b46a8bd66582af7cb45225d349c2 |}]
      ;;

      let of_model t = t
    end
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "unarchive-feature" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
