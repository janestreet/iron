module Stable = struct
  type tick_a [@@deriving bin_io]

  let%expect_test _ =
    print_endline [%bin_digest: tick_a];
    [%expect {| 67e248d3b3dd2a1dc1cffedfb76277f3 |}]
  ;;
end

include Stable
