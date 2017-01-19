module Stable = struct

  open! Import_stable

  module Definition = struct
    module V1 = struct
      type t =
        { typo  : Alternate_name.V1.t
        ; means : User_name.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ba7d93e3bec43f60cda90d2b6c53ce80 |}]
      ;;
    end
  end

  module Action = struct
    module V1 = struct
      type t =
        { definitions         : Definition.V1.t list
        ; may_repartition_crs : bool
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 222ff1e9a6c0578822c9f9d88ded97b7 |}]
      ;;

      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = Unit
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "define-typos" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V1
module Definition = Stable.Definition.V1
