module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path                : Feature_path.V1.t
        ; rev_zero                    : Rev.V1.t
        ; allow_non_cr_clean_new_base : bool
        ; for_                        : User_name.V1.t
        ; new_base                    : Rev.V1.t option
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| e50cf86879dc587b2dc282e364f302db |}]
      ;;

      let to_model t = t
    end
    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t =
        { old_tip          : Rev.V1.t
        ; old_base         : Rev.V1.t
        ; new_base         : Rev.V1.t
        ; remote_repo_path : Remote_repo_path.V1.t
        ; feature_id       : Feature_id.V1.t
        }
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 3c0c4625ae4f4c9e3e4cb5f545e694c2 |}]
      ;;

      let of_model t = t
    end
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "prepare-to-rebase" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
