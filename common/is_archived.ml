module Stable = struct
  open! Core.Core_stable

  module V1 = struct
    type t =
      | Yes of { reason_for_archiving : string }
      | No
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 79213c3a1e14f0b8cb0d6dfaddc00958 |}]
    ;;

    let to_bool = function
      | Yes _ -> true
      | No    -> false
    ;;
  end

  module Model = V1
end

open! Core
open! Import

include Stable.Model
