module Stable = struct
  open! Core.Core_stable

  module Session_id = Session_id.Stable

  module V1 = struct
    type t =
      | Current_session
      | This_session    of Session_id.V1.t
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| b3d35dc794e636a1ec481fa929181aec |}]
    ;;
  end

  module Model = V1
end

include Stable.Model
