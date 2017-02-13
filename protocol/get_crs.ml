module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; for_or_all   : User_name.Or_all.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 3f45aa2499503cc3e3e54794a4eff3f0 |}]
      ;;

      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = struct
      type t = Cr_comment.V1.t list Or_error.V1.t
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 6d1e805dca6779d2378758942a3058f0 |}]
      ;;

      let of_model t = t
    end
  end

end

include Iron_versioned_rpc.Make
    (struct let name = "get-crs" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V1
