module Stable = struct
  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        [ `Aliases
        | `Typos
        | `All
        ]
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 97543f5a927cc53fee5121b3253e3b56 |}]
      ;;

      let to_model t = t
    end

    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t = User_name_by_alternate_name.V1.t
      [@@deriving bin_io, compare, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| aa41ee0fc9442ac7cd1b3ce2e5196e91 |}]
      ;;

      let of_model t = t
    end

    module Model = V1
  end
end

open! Core
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "get-alternate-names" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.  Model
module Reaction = Stable.Reaction.Model
