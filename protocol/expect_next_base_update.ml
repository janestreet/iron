module Stable = struct
  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path  : Feature_path.V1.t
        ; for_          : User_name.V1.t
        ; expected_base : Rev.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| c78cdd91a8ed077c407bf1238dcc0518 |}]
      ;;

      let to_model t = t
    end
    module Model = V1
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

open! Core
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "expect-next-base-update" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.  Model
module Reaction = Stable.Reaction.Model
