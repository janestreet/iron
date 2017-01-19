module Stable = struct

  open! Import_stable

  module Feature = Feature.Stable

  module Action = struct
    module V1 = struct
      type t = Feature_path.V1.t
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 296be80010ace497614f92952e5510c4 |}]
      ;;

      let to_model t = t
    end

    module Model = V1
  end

  module Reaction = struct
    module V2 = struct
      type t = No | Yes of Feature_id.V1.t
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b3581aeb127c4f0c77cb2a2207501e09 |}]
      ;;

      let of_model (t : t) = t
    end

    module V1 = struct
      type t = bool
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| a25306e4c5d30d35adbb5b0462a6b1b3 |}]
      ;;

      let of_model m =
        match V2.of_model m with
        | No -> false
        | Yes (_ : Feature_id.V1.t) -> true
      ;;
    end

    module Model = V2
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "feature-exists" end)
    (struct let version = 2 end)
    (Stable.Action.V1)
    (Stable.Reaction.V2)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
