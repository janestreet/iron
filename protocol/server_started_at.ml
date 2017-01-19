module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = Unit
    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t =
        { server_started_at : Time.V1_round_trippable.t
        }
      [@@deriving bin_io, fields, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 800cfb3238d880922b1137bc166e5ea4 |}]
      ;;

      let of_model t = t
    end
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "server-started-at" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
