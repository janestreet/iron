module Stable = struct
  open Core.Core_stable
  module V1 = struct
    type t = bool
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| a25306e4c5d30d35adbb5b0462a6b1b3 |}]
    ;;
  end
end

open Core
open! Import

include Bool
