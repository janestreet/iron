module Stable = struct

  open! Import_stable

  module V1 = struct
    type t =
      { bookmark        : string
      ; first_12_of_rev : Node_hash.First_12.V1.t
      }
    [@@deriving bin_io, compare, fields, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 3163a889d786202357fbc93aae0f62ba |}]
    ;;
  end
end

include Stable.V1
