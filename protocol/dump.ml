module Stable = struct

  open! Import_stable

  module Which_user_info = struct
    module V1 = struct
      type t =
        | Aliases
        | Existing_users
        | Typos
        | Valid_users
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8631f7d931bd587a341aa557d5337682 |}]
      ;;
    end

    module Model = V1
  end

  module Action = struct
    module V13 = struct
      type t =
        | Archived_features_cache   of [ `Stats
                                       | `Ids_and_feature_paths
                                       | `Value of Feature_id.V1.t
                                       ]
        | Bookmarks_without_feature of Remote_repo_path.V1.t option * User_name.Or_all.V1.t
        | Build_info
        | Event_subscriptions
        | Feature                   of Feature_path.V1.t
        | Hash_consing_cache        of [ `Stats
                                       | `Values
                                       | `Module_values    of string
                                       | `Module_hash_data of string
                                       ]
        | Dynamic_upgrade_state
        | Push_events               of [ `Feature_id of Feature_id.V1.t
                                       | `Stats
                                       | `User_name  of User_name.V1.t
                                       | `Values
                                       ]
        | Review_analysis           of Feature_path.V1.t
        | Review_manager            of Feature_path.V1.t * User_name.Or_all.V1.t
        | Review_lines              of Feature_path.V1.t * User_name.Or_all.V1.t
        | State
        | Timed_event_table
        | Unclean_workspaces        of User_name.Or_all.V1.t
        | User_info                 of Which_user_info.V1.t
        | Version
        | Worker_cache              of [ `Stats
                                       | `Revs
                                       | `Values_at_rev      of Rev.V1.t
                                       | `Obligations_at_rev of Rev.V1.t
                                       ]
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 900b3a875522c85468d1863e04bddbc6 |}]
      ;;

      let to_model t = t
    end

    module Model = V13
  end

  module Reaction = struct
    module V1 = struct
      type t = Sexp.t [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 832b40ae394f2851da8ba67b3339b429 |}]
      ;;

      let of_model t = t
    end

    module Model = V1
  end

end

include Iron_versioned_rpc.Make
    (struct let name = "dump" end)
    (struct let version = 13 end)
    (Stable.Action.V13)
    (Stable.Reaction.V1)

module Which_user_info = Stable.Which_user_info. Model
module Action          = Stable.Action.          Model
module Reaction        = Stable.Reaction.        Model
