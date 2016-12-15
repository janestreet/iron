module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { remote_repo_path : Remote_repo_path.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f02e28f8234664a56eb29c60d2f3d589 |}]
      ;;

      let to_model t = t
    end

    module Model = V1
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "clear-bookmarks-without-feature" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
