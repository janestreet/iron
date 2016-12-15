module Stable = struct

  open Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { rev : Rev.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| a2ef260cd6d4c0ab084d1193287efa60 |}]
      ;;

      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = Unit
  end

end

include Iron_versioned_rpc.Make
    (struct let name = "revision-is-fully-reviewed" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V1
