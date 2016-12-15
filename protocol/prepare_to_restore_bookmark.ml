module Stable = struct

  open Import_stable

  module Feature = Feature.Stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; rev_zero     : Rev.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 25e91f85b75ca8aeb1afa63bb9ba7795 |}]
      ;;

      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = struct
      type t =
        { tip              : Rev.V1.t
        ; remote_repo_path : Remote_repo_path.V1.t
        }
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 002e3541a7b3b8f948dc74fada75b43e |}]
      ;;

      let of_model m = m
    end
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "prepare-to-restore-bookmark" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V1
