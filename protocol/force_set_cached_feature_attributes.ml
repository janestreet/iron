module Stable = struct

  open! Import_stable

  module Action = struct
    module V2 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; skip_post_RPC_check : bool
        ; next_steps          : Next_step.V6.t list
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 31e1666fde16266404b5f0a87c212403 |}]
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
    (struct let name = "force-set-cached-feature-attributes" end)
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
