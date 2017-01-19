module Stable = struct

  open! Import_stable

  module What_to_do = struct
    module V1 = struct
      type t =
        | Add    of Rev.V1.t
        | Remove of Rev.V1.t
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| fff0a40a95d2c005fb462a2d8b45688e |}]
      ;;
    end
  end

  module Action = struct
    module V2 = struct
      type t =
        { what_to_do : What_to_do.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f638ee4b729bee1dab6d2fea511bf1db |}]
      ;;

      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = Unit
  end

end

include Iron_versioned_rpc.Make
    (struct let name = "change-fully-reviewed-revisions" end)
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

module Action     = Stable.Action.V2
module Reaction   = Stable.Reaction.V1
module What_to_do = Stable.What_to_do.V1
