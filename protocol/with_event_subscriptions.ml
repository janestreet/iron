module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        | Set_max_subscriptions_per_user of int
        | Set_max_subscriptions_global   of int
        | Drop_all_by_user               of User_name.Or_all.V1.t
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ef819efd8c84fe0d3374663befc9bc43 |}]
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
    (struct let name = "with-event-subscriptions" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
