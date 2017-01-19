module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t = (Fact.Spec.Id.V1.t * Fact.Scope.V1.t) [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| de04eb9216262166dc055f0a940c4129 |}]
      ;;
      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = struct
      type t = Fact.Evidence.V1.t [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| addc3b0ab00e5bec3a87e09d6d1c3554 |}]
      ;;
      let of_model t = t
    end
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "fact-evidence" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V1
