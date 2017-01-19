module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { alternate_names     : Alternate_name.V1.t list
        ; which               : [ `Aliases | `Typos ]
        ; may_repartition_crs : bool
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 4b750c003471106c3b6ae0078d7e7f17 |}]
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
    (struct let name = "remove-alternate-names" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
