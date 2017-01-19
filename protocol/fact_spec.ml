module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t = Fact.Spec.Id.V1.t [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
      ;;
      let to_model t = t
    end
    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t = Fact.Spec.V1.t [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| a33f47db1e91310e63c3171aab6d5a2a |}]
      ;;
      let of_model t = t
    end
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "fact-spec" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
