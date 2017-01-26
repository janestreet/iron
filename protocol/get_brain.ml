module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; for_         : User_name.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| caa8c6bc0b63d59c3c306d8e2483b33e |}]
      ;;

      let to_model t = t
    end

    module Model = V1
  end

  module Reaction = struct
    module V3 = struct
      type t =
        { reviewer         : Reviewer.V2.t
        ; brain            : Brain.V3.t
        ; remote_rev_zero  : Rev.V1.t
        ; remote_repo_path : Remote_repo_path.V1.t
        }
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| a273209afc783673cb8f1be748af8134 |}]
      ;;

      open! Core
      open! Import

      let of_model m = m
    end

    module Model = V3

    module V2 = struct
      type t =
        { reviewer         : Reviewer.V1.t
        ; brain            : Diff2s.V2.t
        ; remote_rev_zero  : Rev.V1.t
        ; remote_repo_path : Remote_repo_path.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 55da2913122cc8ec37f3f639de598366 |}]
      ;;

      open! Core
      open! Import

      let of_v3 { V3.
                  reviewer
                ; brain
                ; remote_rev_zero
                ; remote_repo_path
                } =
        { reviewer =
            (if reviewer.is_whole_feature_reviewer
             then Whole_feature_reviewer
             else Normal_reviewer reviewer.user_name)
        ; brain    = Brain.Stable.V2.of_model brain
        ; remote_rev_zero
        ; remote_repo_path
        }
      ;;

      let of_model m = of_v3 (V3.of_model m)
    end
  end
end

open! Core
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "get-brain" end)
    (struct let version = 3 end)
    (Stable.Action.V1)
    (Stable.Reaction.V3)

include Register_old_rpc
    (struct let version = 2 end)
    (Stable.Action.V1)
    (Stable.Reaction.V2)

module Action   = Stable.Action.  Model
module Reaction = Stable.Reaction.Model

include Register_map_reaction_in_client (struct
    (* The sort is done client side to avoid unnecessary work in Iron server. *)
    let of_server_reaction (_ : action) (reaction : reaction) =
      let brain : Brain.t =
        List.sort reaction.brain ~cmp:(fun (t1 : Brain.Marked_diff2.t) t2 ->
          Path_in_repo.default_review_compare
            (Diff2.path_in_repo_at_tip t1.diff2)
            (Diff2.path_in_repo_at_tip t2.diff2))
      in
      { reaction with brain }
    ;;
  end)
