module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; lock_names   : Lock_name.V2.t list
        ; reason       : string
        ; is_permanent : bool
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| d56ad0ba8e38e68e55bf7352c9099812 |}]
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
    (struct let name = "lock" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
