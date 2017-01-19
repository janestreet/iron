module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { which_features         : Which_features.V1.t
        ; ignore_diffs_in_errors : bool
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 7e077de6ad12dff92775eceb58807626 |}]
      ;;

      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = Unit
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "check-cached-feature-attributes" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.  V1
module Reaction = Stable.Reaction.V1
