module Stable = struct

  open! Import_stable

  module Action = struct
    module V2 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; when_to_first_notify : When_to_first_notify.V1.t
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 771fa04633bd79719ec15951315b05a7 |}]
      ;;

      let to_model (t : t) = t
    end

    module V1 = struct
      type t = Feature_path.V1.t
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 296be80010ace497614f92952e5510c4 |}]
      ;;

      let to_model feature_path =
        V2.to_model
          { feature_path
          ; when_to_first_notify = At_next_change
          }
      ;;
    end

    module Model = V2
  end

  module Reaction = struct
    module V1 = struct
      type t =
        [ `Updates_in_subtree
        | `Archived
        | `Renamed
        ]
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 3fa50667ae44a268a34c4c0433f959b2 |}]
      ;;

      let of_model t = t
    end

    module Model = V1
  end
end

include Iron_versioned_rpc.Make_pipe_rpc
    (struct let name = "notify-on-descendant-updates" end)
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
