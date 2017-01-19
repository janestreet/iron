module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t = Feature_path.V1.t
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 296be80010ace497614f92952e5510c4 |}]
      ;;

      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = Unit
  end

end

include Iron_versioned_rpc.Make
    (struct let name = "force-retry" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V1
