module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module V2 = struct
    type t =
      { feature_id : Feature_id.V1.t
      ; from       : Feature_path.V1.t
      ; to_        : Feature_path.V1.t
      }
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| d745589d5f8eedd0376f4118657340f9 |}]
    ;;
  end

  module V1 = struct
    type t =
      { from : Feature_path.V1.t
      ; to_  : Feature_path.V1.t
      }
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| ef36ab97563f6e9376fed8899f0d88b3 |}]
    ;;

    let of_v2 { V2.feature_id = _; from; to_ } = { from; to_ }
  end
end

include Stable.V2
