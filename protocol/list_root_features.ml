module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = Unit
    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type one =
        { root_feature     : Feature_name.V1.t
        ; remote_repo_path : Remote_repo_path.V1.t
        ; tip              : Rev.V1.t
        }
      [@@deriving bin_io, fields, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: one];
        [%expect {| 74b7928a0efc8eaa1107c0d1c4f7a948 |}]
      ;;

      type t = one list
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 7d85e258aaa9975b40c6c0f3258274c0 |}]
      ;;

      let of_model t = t
    end
    module Model = V1
  end
end

open! Core
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "list-root-features" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model

include Register_map_reaction_in_client
    (struct
      (* The sort is done client side to avoid unnecessary work in Iron server. *)
      let of_server_reaction (_ : action) (reaction : reaction) =
        List.sort reaction ~cmp:(fun (t1 : Reaction.one) t2 ->
          Feature_name.compare t1.root_feature t2.root_feature)
      ;;
    end)
