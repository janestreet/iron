module Stable = struct

  open! Core.Core_stable

  module Feature_path = Feature_path.Stable

  module V1 = struct
    type t =
      | Any_root
      | Feature  of Feature_path.V1.t
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 35bd14e149233f5ed021100e18a3f1f0 |}]
    ;;

    let to_model m = m
    let of_model m = m
  end

  module Model = V1
end

open! Core
open! Import

include Stable.Model
