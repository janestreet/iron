module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { from           : Feature_path.V1.t
        ; to_            : Feature_path.V1.t
        ; skip_gca_check : bool
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 4d8b1d696ca323ec1dabc17db1bc0911 |}]
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
    (struct let name = "rename" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
