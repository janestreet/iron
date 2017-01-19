module Stable = struct

  open! Import_stable

  module Action = struct
    module V3 = struct
      type t =
        | Clear_features        of Which_features.V1.t
        | Clear_revs            of Rev.V1.t list
        | Set_max_size          of int
        | Set_status            of Worker_cache.Status.V1.t
        | Set_max_items_per_rpc of int
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8ca1f9d8630f8e3587b539b571f2f810 |}]
      ;;

      let to_model t = t
    end
    module Model = V3
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "with-worker-cache" end)
    (struct let version = 3 end)
    (Stable.Action.V3)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
