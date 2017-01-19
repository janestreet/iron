module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path           : Feature_path.V1.t
        ; for_                   : User_name.V1.t
        ; ok_if_nothing_cleared  : bool
        ; only_those_reviewed_by : Unresolved_name.V1.t Blang.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ba904642f373f7a5ff89cbe1b611ab94 |}]
      ;;

      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = Unit
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "clear-catch-up-sessions" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V1
