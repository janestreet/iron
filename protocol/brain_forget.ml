module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path   : Feature_path.V1.t
        ; for_           : User_name.V1.t
        ; what_to_forget : [ `All
                           | `Files of Path_in_repo.V1.t list
                           ]
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f336872c1a7827c571cea4b9ba482912 |}]
      ;;

      let to_model t = t
    end
  end

  module Reaction = struct
    module V1 = Unit
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "brain-forget" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V1
module Reaction = Stable.Reaction.V1
