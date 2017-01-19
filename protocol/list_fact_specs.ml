module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = Unit
    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t = (Fact.Spec.Id.V1.t * Fact.Spec.V1.t) list [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 46bd978871a35e0e3650fcdf5518215a |}]
      ;;

      let of_model t = t
    end
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "fact-specs" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
