module Stable = struct
  module V1 = struct
    type t =
      | Now
      | At_next_change
    [@@deriving bin_io, compare, enumerate, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| b15bb375f83591e63836046593dad724 |}]
    ;;
  end
end

open! Core
open! Import

module T = Stable.V1
include T
include Comparable.Make (T)
