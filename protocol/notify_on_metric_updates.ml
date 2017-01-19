module Stable = struct
  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        | Feature_path of Feature_path.V1.t
        | Metric_name  of Metric_name.V1.t
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 5a23fb3a2b5abe083f9372427dc46e8f |}]
      ;;

      let to_model m = m
    end

    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; metric_name  : Metric_name.V1.t
        ; value        : float
        ; added_at     : Time.V1_round_trippable.t
        }
      [@@deriving bin_io, fields, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 12c23082d186da8ecab7858c5fc2d5e9 |}]
      ;;

      let of_model m = m
    end

    module Model = V1
  end
end

include Iron_versioned_rpc.Make_pipe_rpc
    (struct let name = "notify-on-metric-updates" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
