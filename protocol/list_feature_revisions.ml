module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { rev_zero : Rev.V1.t
        ; features : Feature_path.V1.t list
        ; subtrees : Feature_path.V1.t list
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| e191b6916910031bea4297c326414ddd |}]
      ;;

      let to_model t = t
    end
    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type one =
        { feature_path : Feature_path.V1.t
        ; base         : Rev.V1.t
        ; tip          : Rev.V1.t
        }
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: one];
        [%expect {| e1d86d8f6b3e1ad8d4099309d9c8ad6c |}]
      ;;

      type t =
        { remote_repo_path : Remote_repo_path.V1.t
        ; features         : one list
        }
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 5f627e3902137ef81acdb24c97ab684b |}]
      ;;

      let of_model t = t
    end
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "list-feature-revisions" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
