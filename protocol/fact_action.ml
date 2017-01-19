module Stable = struct

  open! Import_stable

  module Action = struct
    module V2 = struct
      type t = Fact.Action.V2.t [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 26463e805cec97a9c8a6ba8337f9dcaf |}]
      ;;

      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = Unit
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "fact-action" end)
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V2
module Reaction = Stable.Reaction.V1
