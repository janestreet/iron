module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        | Remove_user    of User_name.V1.t
        | Remove_machine of User_name.V1.t * Machine_name.V1.t
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 69c0fd20f0fc7de9ff23d9600f193a83 |}]
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
    (struct let name = "with-unclean-workspaces" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
