module Stable = struct

  open! Import_stable

  module Action = struct
    module V4 = struct
      type t =
        { feature_path      : Feature_path.V1.t
        ; even_though_empty : bool
        ; even_though_owner : bool
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 7ff04ba3148a46af4f8d6c86ab5c1742 |}]
      ;;

      let to_model (t : t) = t
    end

    module V3 = struct
      type t =
        { feature_path      : Feature_path.V1.t
        ; for_              : User_name.V1.t
        ; even_though_empty : bool
        ; even_though_owner : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 5cb55e299a244f539a71ba80e59b483f |}]
      ;;

      let to_model { feature_path; even_though_empty; even_though_owner; _ } =
        V4.to_model { feature_path; even_though_empty; even_though_owner }
      ;;
    end

    module Model = V4
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "second" end)
    (struct let version = 4 end)
    (Stable.Action.V4)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 3 end)
    (Stable.Action.V3)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
