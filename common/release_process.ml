module Stable = struct
  module V1 = struct
    type t = Continuous | Direct
    [@@deriving bin_io, compare, enumerate, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| f895be624cf2e74fe7f1b7c54d8120fe |}]
    ;;
  end
end

open! Core
open! Import

include Stable.V1

let to_string_hum = function
  | Continuous -> "continuous"
  | Direct     -> "direct"
;;

let equal t t' = compare t t' = 0
