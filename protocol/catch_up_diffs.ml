module Stable = struct

  open! Import_stable

  (* [for_] is for testing. Catch_up on behalf of someone else will be rejected if not in
     test mode *)
  module Action = struct
    module V1 = struct
      type t =
        { feature_path         : Feature_path.V1.t
        ; for_                 : User_name.V1.t
        ; catch_up_session_id  : Session_id.V1.t
        ; diff4_in_session_ids : Diff4_in_session.Id.V1.t list
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 0d5d00e68f0c35f0060bf15d1ba933de |}]
      ;;

      let to_model t = t
    end
    module Model = V1
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "catch-up-diffs" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
