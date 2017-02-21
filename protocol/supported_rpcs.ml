module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = Unit
    module Model = V1
  end

  module Reaction = struct
    module V2 = struct
      type t = Rpc_description.V2.t list
      [@@deriving bin_io, compare, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8efe5ea269d6f428aa74c773b6299034 |}]
      ;;

      let of_model (m : t) = m
    end

    module V1 = struct
      type t = Rpc_description.V1.t list
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 9301ea9546114195a8bd36b8e8d4dd97 |}]
      ;;

      open! Core

      let of_model m =
        List.map (V2.of_model m) ~f:Rpc_description.V1.of_v2
      ;;

    end
    module Model = V2
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "supported-rpcs" end)
    (struct let version = 2 end)
    (Stable.Action.V1)
    (Stable.Reaction.V2)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
