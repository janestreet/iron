module Stable = struct

  open! Import_stable

  module Which_user_info = struct
    module V1 = struct
      type t =
        | Alias
        | Existing_user
        | Typo
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| bdb826fc98c4b919e09992038bdc02ad |}]
      ;;
    end

    module Model = V1
  end

  module Type = struct

    module V7 = struct
      type t =
        | Absolute_feature_path
        | Archived_feature_path
        | Feature_path
        | Feature_path_with_catch_up
        | Metric_name
        | Remote_repo_path
        | Root_feature_path
        | User_info of Which_user_info.V1.t
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 0a7e93070a1f2af865657e703ff5d5c7 |}]
      ;;

    end

    module V6 = struct
      type t =
        | Absolute_feature_path
        | Archived_feature_path
        | Feature_path
        | Feature_path_with_catch_up
        | Remote_repo_path
        | Root_feature_path
        | User_info of Which_user_info.V1.t
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| dfa33772bab598fec41bf056e7bf8887 |}]
      ;;

      let to_v7 = function
        | Absolute_feature_path      -> V7.Absolute_feature_path
        | Archived_feature_path      -> V7.Archived_feature_path
        | Feature_path               -> V7.Feature_path
        | Feature_path_with_catch_up -> V7.Feature_path_with_catch_up
        | Remote_repo_path           -> V7.Remote_repo_path
        | Root_feature_path          -> V7.Root_feature_path
        | User_info which_user       -> V7.User_info which_user
      ;;
    end

    module V5 = struct
      type t =
        | Absolute_feature_path
        | Archived_feature_path
        | Feature_path
        | Remote_repo_path
        | Root_feature_path
        | User_info of Which_user_info.V1.t
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 4ceeb065ada4a71dba2c76bf80f0e130 |}]
      ;;

      let to_v6 = function
        | Absolute_feature_path -> V6.Absolute_feature_path
        | Archived_feature_path -> V6.Archived_feature_path
        | Feature_path          -> V6.Feature_path
        | Remote_repo_path      -> V6.Remote_repo_path
        | Root_feature_path     -> V6.Root_feature_path
        | User_info which_user  -> V6.User_info which_user
      ;;
    end

    module V4 = struct
      type t =
        | Absolute_feature_path
        | Archived_feature_path
        | Feature_path
        | Remote_repo_path
        | User_info of Which_user_info.V1.t
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| aa8a7d63883c8edfca9fb7b6c29f1683 |}]
      ;;

      let to_v5 = function
        | Absolute_feature_path -> V5.Absolute_feature_path
        | Archived_feature_path -> V5.Archived_feature_path
        | Feature_path          -> V5.Feature_path
        | Remote_repo_path      -> V5.Remote_repo_path
        | User_info which_user  -> V5.User_info which_user
      ;;
    end

    module Model = V7
  end

  module Action = struct
    module V7 = struct
      type t =
        { types  : Type.V7.t list
        ; prefix : string
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ddaa97a4d78215a21cb7763f97d16d76 |}]
      ;;

      let to_model (t : t) = t
    end

    module V6 = struct
      type t =
        { types  : Type.V6.t list
        ; prefix : string
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| aafdbfc0c414dc9d08bdc7c276407358 |}]
      ;;

      let to_model { types; prefix } =
        V7.to_model
          { types  = List.map types ~f:Type.V6.to_v7
          ; prefix
          }
      ;;
    end

    module V5 = struct
      type t =
        { types  : Type.V5.t list
        ; prefix : string
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8e2c2e2a5ec1138f5af43532e6a6b80d |}]
      ;;

      let to_model { types; prefix } =
        V6.to_model
          { types  = List.map types ~f:Type.V5.to_v6
          ; prefix
          }
      ;;
    end

    module V4 = struct
      type t =
        { type_  : Type.V4.t
        ; prefix : string
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 36da190aa0400c34f818fdb275137670 |}]
      ;;

      let to_model { type_; prefix } =
        V5.to_model
          { types = [ Type.V4.to_v5 type_ ]
          ; prefix
          }
      ;;
    end

    module Model = V7
  end

  module Reaction = struct
    module V1 = struct
      type t = string list [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 296be80010ace497614f92952e5510c4 |}]
      ;;

      let of_model t = t
    end

    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "complete" end)
    (struct let version = 7 end)
    (Stable.Action.V7)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 6 end)
    (Stable.Action.V6)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 5 end)
    (Stable.Action.V5)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 4 end)
    (Stable.Action.V4)
    (Stable.Reaction.V1)

module Action          = Stable.Action.          Model
module Reaction        = Stable.Reaction.        Model
module Type            = Stable.Type.            Model
module Which_user_info = Stable.Which_user_info. Model
