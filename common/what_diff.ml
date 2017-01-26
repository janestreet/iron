module Stable = struct
  module User_name = User_name.Stable

  module V2 = struct
    type t =
      | Whole_diff
      | Whole_diff_plus_ignored
      | For of User_name.V1.t
      | None
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 4f0e2d8c3f3baca57cfefc5b0965ea43 |}]
    ;;

    let to_model t = t
  end

  module V1 = struct
    type t =
      | Whole_diff
      | Whole_diff_plus_ignored
      | For of User_name.V1.t
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 6cb12238e519c9575f95ec051ed4a116 |}]
    ;;

    let to_v2 = function
      | Whole_diff              -> V2.Whole_diff
      | Whole_diff_plus_ignored -> Whole_diff_plus_ignored
      | For user                -> For user
    ;;

    let to_model t = V2.to_model (to_v2 t)
  end

  module Model = V2
end

open! Core
open! Import

include Stable.Model
