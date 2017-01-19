module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; feature_id   : Feature_id.V1.t option
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 9fe95d0d182485c6e494bf9a11bc7081 |}]
      ;;

      let to_model m = m
    end
    module Model = V1
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

include Iron_command_rpc.Make
    (struct let name = "unarchive" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
