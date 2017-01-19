module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = Unit

    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t = Rpc_description.V1.t list
      [@@deriving bin_io, compare, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 9301ea9546114195a8bd36b8e8d4dd97 |}]
      ;;

      let of_model m = m
    end

    module Model = V1
  end
end

include Iron_command_rpc.Make
    (struct let name = "supported-command-rpcs" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
