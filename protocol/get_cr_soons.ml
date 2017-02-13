module Stable = struct

  open! Import_stable

  module Action = struct
    module V2 = struct
      type t =
        { root_feature   : Feature_name.V1.t
        ; for_or_all     : User_name.Or_all.V1.t
        ; include_active : bool
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 3e77db419a99f7a4c4337936436937dc |}]
      ;;

      let to_model t = t
    end
    module Model = V2
  end

  module Reaction = struct
    module V1 = struct
      type t = Cr_soon_multiset.V1.t [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 6a97a4c3fe9fa833dcf1d598e874eea3 |}]
      ;;

      let of_model t = t
    end
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "get-cr-soons" end)
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
