module Stable = struct

  open! Import_stable

  module Bookmark_without_feature = Bookmark_without_feature.Stable

  module Action = struct
    module V3 = struct
      type t =
        { for_                       : User_name.V1.t
        ; include_active_cr_soons    : bool
        ; include_all_owned_features : bool
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 2f4a1c28d3128057e7e2a31173868dd5 |}]
      ;;

      let to_model t = t
    end

    module Model = V3

    module V2 = struct
      type t =
        { for_                    : User_name.V1.t
        ; include_active_cr_soons : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| e62afa8bd8b58b95a074f480b2dddc6d |}]
      ;;

      let to_model { for_; include_active_cr_soons } =
        V3.to_model { V3.
                      for_
                    ; include_active_cr_soons
                    ; include_all_owned_features = true
                    }
      ;;
    end

  end

  module Num_crs = struct
    module V1 = struct
      type t =
        [ `Enabled of int Or_error.V1.t
        | `Disabled
        ]
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ec5ff7fed9be1ddad772245eba990a4c |}]
      ;;
    end

    module Model = V1
  end

  module Review_lines = struct
    module V1 = struct
      type t =
        { review : Review_or_commit.V1.t
        ; follow : int
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 92940aeddac590117bcf32a5ad4d76ae |}]
      ;;
    end
  end

  module Assigned = struct

    module V11 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; feature_path_exists : bool
        ; review_is_enabled   : bool
        ; user_is_reviewing   : bool
        ; assigned_next_steps : Next_step.V6.t list
        ; num_crs             : Num_crs.V1.t
        ; num_xcrs            : Num_crs.V1.t
        ; line_count          : Line_count.V5.t
        ; next_steps          : Next_step.V6.t list
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b52def8e14823cd0130f53778c7ad0da |}]
      ;;
    end

    module V10 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; feature_path_exists : bool
        ; review_is_enabled   : bool
        ; user_is_reviewing   : bool
        ; may_second          : bool
        ; num_crs             : Num_crs.V1.t
        ; num_xcrs            : Num_crs.V1.t
        ; line_count          : Line_count.V5.t
        ; next_steps          : Next_step.V6.t list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 7cac5c26366796280ebed11700ee3d64 |}]
      ;;

      let of_v11 { V11.
                   feature_path
                 ; feature_path_exists
                 ; review_is_enabled
                 ; user_is_reviewing
                 ; assigned_next_steps
                 ; num_crs
                 ; num_xcrs
                 ; line_count
                 ; next_steps
                 } =
        let may_second =
          List.exists assigned_next_steps ~f:(function
            | Ask_seconder -> true
            | _ -> false)
        in
        { feature_path
        ; feature_path_exists
        ; review_is_enabled
        ; user_is_reviewing
        ; may_second
        ; num_crs
        ; num_xcrs
        ; line_count
        ; next_steps
        }
      ;;
    end

    module V9 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; feature_path_exists : bool
        ; review_is_enabled   : bool
        ; user_is_reviewing   : bool
        ; may_second          : bool
        ; num_crs             : Num_crs.V1.t
        ; num_xcrs            : Num_crs.V1.t
        ; line_count          : Line_count.V5.t
        ; next_steps          : Next_step.V5.t list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f43585b08840ff214707b2763f9826ea |}]
      ;;

      open! Core
      open! Import

      let of_v10 { V10.
                   feature_path
                 ; feature_path_exists
                 ; review_is_enabled
                 ; user_is_reviewing
                 ; may_second
                 ; num_crs
                 ; num_xcrs
                 ; line_count
                 ; next_steps
                 } =
        let next_steps = List.map next_steps ~f:Next_step.Stable.V5.of_v6 in
        { feature_path
        ; feature_path_exists
        ; review_is_enabled
        ; user_is_reviewing
        ; may_second
        ; num_crs
        ; num_xcrs
        ; line_count
        ; next_steps
        }
      ;;
    end

    module V8 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; feature_path_exists : bool
        ; review_is_enabled   : bool
        ; user_is_reviewing   : bool
        ; may_second          : bool
        ; num_crs             : Num_crs.V1.t
        ; num_xcrs            : Num_crs.V1.t
        ; line_count          : Line_count.V4.t
        ; next_steps          : Next_step.V5.t list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 58f930b02db46cfb003db32d46befe50 |}]
      ;;

      let of_v9 { V9.
                  feature_path
                ; feature_path_exists
                ; review_is_enabled
                ; user_is_reviewing
                ; may_second
                ; num_crs
                ; num_xcrs
                ; line_count
                ; next_steps
                } =
        let line_count = Line_count.V4.of_v5 line_count in
        { feature_path
        ; feature_path_exists
        ; review_is_enabled
        ; user_is_reviewing
        ; may_second
        ; num_crs
        ; num_xcrs
        ; line_count
        ; next_steps
        }
      ;;
    end

    module V7 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; feature_path_exists : bool
        ; review_is_enabled   : bool
        ; may_second          : bool
        ; num_crs             : Num_crs.V1.t
        ; num_xcrs            : Num_crs.V1.t
        ; review_lines        : [ `Lines of Review_lines.V1.t
                                | `Not_reviewing
                                ]
        ; catch_up_lines      : int Or_error.V1.t
        ; next_steps          : Next_step.V5.t list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 9beb77400695588c439d57ad2c6c602e |}]
      ;;

      open! Core
      open! Import

      let of_v8 { V8.
                  feature_path
                ; feature_path_exists
                ; review_is_enabled
                ; user_is_reviewing
                ; may_second
                ; num_crs
                ; num_xcrs
                ; line_count
                ; next_steps
                } =
        let review_lines =
          if not user_is_reviewing
          then `Not_reviewing
          else (
            let review =
              Line_count.Review.to_review_column_shown line_count.review
                ~have_potentially_blocking_review_session_in_progress:
                  line_count.have_uncommitted_and_potentially_blocking_session
            in
            `Lines { Review_lines.V1.
                     review
                   ; follow = line_count.review.follow
                   })
        in
        let catch_up_lines = Ok (Line_count.Catch_up.total line_count.catch_up) in
        { feature_path
        ; feature_path_exists
        ; review_is_enabled
        ; may_second
        ; num_crs
        ; num_xcrs
        ; review_lines
        ; catch_up_lines
        ; next_steps
        }
      ;;
    end

    module V6 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; feature_path_exists : bool
        ; review_is_enabled   : bool
        ; may_second          : bool
        ; num_crs             : Num_crs.V1.t
        ; num_xcrs            : Num_crs.V1.t
        ; review_lines        : [ `Lines of Review_lines.V1.t
                                | `Not_reviewing
                                ]
        ; catch_up_lines      : int Or_error.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| acfad65e6151fb7d0d8bef864b5785d5 |}]
      ;;

      let of_v7 { V7.
                  feature_path
                ; feature_path_exists
                ; review_is_enabled
                ; may_second
                ; num_crs
                ; num_xcrs
                ; review_lines
                ; catch_up_lines
                ; next_steps = _
                } =
        { feature_path
        ; feature_path_exists
        ; review_is_enabled
        ; may_second
        ; num_crs
        ; num_xcrs
        ; review_lines
        ; catch_up_lines
        }
      ;;
    end

    module V5 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; feature_path_exists : bool
        ; review_is_enabled   : bool
        ; may_second          : bool
        ; num_crs             : Num_crs.V1.t
        ; num_xcrs            : Num_crs.V1.t
        ; review_lines        : [ `Lines of int
                                | `Not_reviewing
                                ]
        ; catch_up_lines      : int Or_error.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| d0195345c13bdc4093608a140cdd4295 |}]
      ;;

      open! Core
      open! Import

      let of_v6 { V6.
                  feature_path
                ; feature_path_exists
                ; review_is_enabled
                ; may_second
                ; num_crs
                ; num_xcrs
                ; review_lines
                ; catch_up_lines
                } =
        let review_lines =
          match review_lines with
          | `Not_reviewing            -> `Not_reviewing
          | `Lines { review; follow } ->
            `Lines (Review_or_commit.count review + follow)
        in
        { feature_path
        ; feature_path_exists
        ; review_is_enabled
        ; may_second
        ; num_crs
        ; num_xcrs
        ; review_lines
        ; catch_up_lines
        }
      ;;
    end

    module V4 = struct
      type t =
        { feature_path        : Feature_path.V1.t
        ; feature_path_exists : bool
        ; review_is_enabled   : bool
        ; may_second          : bool
        ; num_crs             : int Or_error.V1.t
        ; num_xcrs            : int Or_error.V1.t
        ; review_lines        : [ `Lines of int
                                | `Not_reviewing
                                ]
        ; catch_up_lines      : int Or_error.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 584e4b4aa384b27560547cde5409912d |}]
      ;;

      let num_crs_of_v5 = function
        | `Enabled crs -> crs
        | `Disabled -> Result.V1.Ok 0
      ;;

      let of_v5 { V5.
                  feature_path
                ; feature_path_exists
                ; review_is_enabled
                ; may_second
                ; num_crs
                ; num_xcrs
                ; review_lines
                ; catch_up_lines
                } =
        { feature_path
        ; feature_path_exists
        ; review_is_enabled
        ; may_second
        ; num_crs           = num_crs_of_v5 num_crs
        ; num_xcrs          = num_crs_of_v5 num_xcrs
        ; review_lines
        ; catch_up_lines
        }
      ;;
    end

    module Model = V11
  end

  module Rev_facts = struct
    module V1 = struct
      type t =
        { is_conflict_free      : (bool, unit) Result.V1.t
        ; is_cr_clean           : (bool, unit) Result.V1.t
        ; obligations_are_valid : (bool, unit) Result.V1.t
        }
      [@@deriving bin_io, compare, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 759dbd1b1f3318a1f28a82a3ac91d58a |}]
      ;;
    end

    module Model = V1
  end

  module Feature_info = struct
    module V7 = struct
      type t =
        { feature_path                        : Feature_path.V1.t
        ; num_crs                             : int Or_error.V2.t
        ; num_xcrs                            : int Or_error.V2.t
        ; num_reviewers_with_review_remaining : int Or_error.V2.t
        ; base                                : Rev_facts.V1.t Or_pending.V1.t
        ; tip                                 : Rev_facts.V1.t Or_pending.V1.t
        ; review_is_enabled                   : bool
        ; next_steps                          : Next_step.V6.t list
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f399131d4228d298eace72bf2ee8db3e |}]
      ;;
    end

    module V6 = struct
      type t =
        { feature_path                        : Feature_path.V1.t
        ; num_crs                             : int Or_error.V1.t
        ; num_xcrs                            : int Or_error.V1.t
        ; num_reviewers_with_review_remaining : int Or_error.V1.t
        ; base                                : Rev_facts.V1.t Or_pending.V1.t
        ; tip                                 : Rev_facts.V1.t Or_pending.V1.t
        ; review_is_enabled                   : bool
        ; next_steps                          : Next_step.V5.t list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 054c28ea9767a4f4f7a5347969e2178c |}]
      ;;

      open! Core
      open! Import

      let of_v7 { V7.
                  feature_path
                ; num_crs
                ; num_xcrs
                ; num_reviewers_with_review_remaining
                ; base
                ; tip
                ; review_is_enabled
                ; next_steps
                } =
        let next_steps = List.map next_steps ~f:Next_step.Stable.V5.of_v6 in
        { feature_path
        ; num_crs
        ; num_xcrs
        ; num_reviewers_with_review_remaining
        ; base
        ; tip
        ; review_is_enabled
        ; next_steps
        }
      ;;
    end

    module Model = V7
  end

  module Reaction = struct

    module V16 = struct
      type t =
        { assigned                  : Assigned.V11.t list
        ; unclean_workspaces        : Unclean_workspace.V2.t list Machine_name.V1.Map.t
        ; owned                     : Feature_info.V7.t list
        ; watched                   : Feature_info.V7.t list
        ; cr_soons                  : Cr_soon_multiset.V1.t
        ; bookmarks_without_feature : (Remote_repo_path.V1.t
                                       * Bookmark_without_feature.V1.t list) list
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| af350bd796405912874eca557a29955a |}]
      ;;

      let of_model (m : t) = m
    end

    module V15 = struct
      type t =
        { assigned                  : Assigned.V10.t list
        ; unclean_workspaces        : Unclean_workspace.V2.t list Machine_name.V1.Map.t
        ; owned                     : Feature_info.V7.t list
        ; watched                   : Feature_info.V7.t list
        ; cr_soons                  : Cr_soon_multiset.V1.t
        ; bookmarks_without_feature : (Remote_repo_path.V1.t
                                       * Bookmark_without_feature.V1.t list) list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8c78ba3a4e9383271e461886ea916712 |}]
      ;;

      let of_model m =
        let { V16.
              assigned
            ; unclean_workspaces
            ; owned
            ; watched
            ; cr_soons
            ; bookmarks_without_feature
            } = V16.of_model m in
        let assigned = List.map assigned ~f:Assigned.V10.of_v11 in
        { assigned
        ; unclean_workspaces
        ; owned
        ; watched
        ; cr_soons
        ; bookmarks_without_feature
        }
      ;;
    end

    module V14 = struct
      type t =
        { assigned                  : Assigned.V9.t list
        ; unclean_workspaces        : Unclean_workspace.V1.t list Machine_name.V1.Map.t
        ; owned                     : Feature_info.V6.t list
        ; watched                   : Feature_info.V6.t list
        ; cr_soons                  : Cr_soon_multiset.V1.t
        ; bookmarks_without_feature : (Remote_repo_path.V1.t
                                       * Bookmark_without_feature.V1.t list) list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 0acf850f22a50e42fa83747b841995a3 |}]
      ;;

      open! Core
      open! Import

      let of_model m =
        let { V15.
              assigned
            ; unclean_workspaces
            ; owned
            ; watched
            ; cr_soons
            ; bookmarks_without_feature
            } = V15.of_model m in
        let assigned = List.map assigned ~f:Assigned.V9.of_v10 in
        let unclean_workspaces =
          Map.map unclean_workspaces ~f:(List.map ~f:Unclean_workspace.Stable.V1.of_v2)
        in
        let owned = List.map owned ~f:Feature_info.V6.of_v7 in
        let watched = List.map watched ~f:Feature_info.V6.of_v7 in
        { assigned
        ; unclean_workspaces
        ; owned
        ; watched
        ; cr_soons
        ; bookmarks_without_feature
        }
      ;;
    end

    module V13 = struct
      type t =
        { assigned                  : Assigned.V8.t list
        ; unclean_workspaces        : Unclean_workspace.V1.t list Machine_name.V1.Map.t
        ; owned                     : Feature_info.V6.t list
        ; watched                   : Feature_info.V6.t list
        ; cr_soons                  : Cr_soon_multiset.V1.t
        ; bookmarks_without_feature : (Remote_repo_path.V1.t
                                       * Bookmark_without_feature.V1.t list) list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8e0050d632df4c510ecbe3a3a8782bd1 |}]
      ;;

      let of_model m =
        let { V14.
              assigned
            ; unclean_workspaces
            ; owned
            ; watched
            ; cr_soons
            ; bookmarks_without_feature
            } = V14.of_model m in
        { assigned = List.map assigned ~f:Assigned.V8.of_v9
        ; unclean_workspaces
        ; owned
        ; watched
        ; cr_soons
        ; bookmarks_without_feature
        }
      ;;
    end

    module V12 = struct
      type t =
        { assigned                  : Assigned.V7.t list
        ; unclean_workspaces        : Unclean_workspace.V1.t list Machine_name.V1.Map.t
        ; owned                     : Feature_info.V6.t list
        ; watched                   : Feature_info.V6.t list
        ; cr_soons                  : Cr_soon_multiset.V1.t
        ; bookmarks_without_feature : (Remote_repo_path.V1.t
                                       * Bookmark_without_feature.V1.t list) list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b5e07d3d7137d28184dbdae211512d9f |}]
      ;;

      let of_model m =
        let { V13.
              assigned
            ; unclean_workspaces
            ; owned
            ; watched
            ; cr_soons
            ; bookmarks_without_feature
            } = V13.of_model m in
        { assigned = List.map assigned ~f:Assigned.V7.of_v8
        ; unclean_workspaces
        ; owned
        ; watched
        ; cr_soons
        ; bookmarks_without_feature
        }
      ;;
    end

    module V11 = struct
      type t =
        { assigned                  : Assigned.V7.t list
        ; owned                     : Feature_info.V6.t list
        ; watched                   : Feature_info.V6.t list
        ; cr_soons                  : Cr_soon_multiset.V1.t
        ; bookmarks_without_feature : (Remote_repo_path.V1.t
                                       * Bookmark_without_feature.V1.t list) list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 414a583604f1d654432daf9f14b39b82 |}]
      ;;

      let of_model m =
        let { V12.
              assigned
            ; owned
            ; watched
            ; cr_soons
            ; bookmarks_without_feature
            ; _
            } = V12.of_model m in
        { assigned
        ; owned
        ; watched
        ; cr_soons
        ; bookmarks_without_feature
        }
      ;;
    end

    module V10 = struct
      type t =
        { assigned                  : Assigned.V6.t list
        ; owned                     : Feature_info.V6.t list
        ; watched                   : Feature_info.V6.t list
        ; cr_soons                  : Cr_soon_multiset.V1.t
        ; bookmarks_without_feature : (Remote_repo_path.V1.t
                                       * Bookmark_without_feature.V1.t list) list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b4ccebcc43ca62377641735f55afb4d2 |}]
      ;;

      let of_model m =
        let { V11.
              assigned
            ; owned
            ; watched
            ; cr_soons
            ; bookmarks_without_feature
            } = V11.of_model m in
        { assigned                  = List.map assigned ~f:Assigned.V6.of_v7
        ; owned
        ; watched
        ; cr_soons
        ; bookmarks_without_feature
        }
      ;;
    end

    module V9 = struct
      type t =
        { assigned                  : Assigned.V5.t list
        ; owned                     : Feature_info.V6.t list
        ; cr_soons                  : Cr_soon_multiset.V1.t
        ; bookmarks_without_feature : (Remote_repo_path.V1.t
                                       * Bookmark_without_feature.V1.t list) list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| bdb8e35d4ebfd6d5e507db1ca0ef9652 |}]
      ;;

      let of_model m =
        let { V10.
              assigned
            ; owned
            ; cr_soons
            ; bookmarks_without_feature
            ; _
            } = V10.of_model m in
        { assigned                  = List.map assigned ~f:Assigned.V5.of_v6
        ; owned
        ; cr_soons
        ; bookmarks_without_feature
        }
      ;;
    end

    module V8 = struct
      type t =
        { assigned                  : Assigned.V4.t list
        ; owned                     : Feature_info.V6.t list
        ; cr_soons                  : Cr_soon_multiset.V1.t
        ; bookmarks_without_feature : (Remote_repo_path.V1.t
                                       * Bookmark_without_feature.V1.t list) list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 7f0519b1f11f273a5a2a2f3762ea6212 |}]
      ;;

      let of_model m =
        let { V9.
              assigned
            ; owned
            ; cr_soons
            ; bookmarks_without_feature
            } = V9.of_model m in
        { assigned                  = List.map assigned ~f:Assigned.V4.of_v5
        ; owned
        ; cr_soons
        ; bookmarks_without_feature
        }
      ;;
    end

    module Model = V16
  end
end

open! Core
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "todo" end)
    (struct let version = 17 end)
    (Stable.Action.V3)
    (Stable.Reaction.V16)

include Register_old_rpc
    (struct let version = 16 end)
    (Stable.Action.V3)
    (Stable.Reaction.V15)

include Register_old_rpc
    (struct let version = 15 end)
    (Stable.Action.V3)
    (Stable.Reaction.V14)

include Register_old_rpc
    (struct let version = 14 end)
    (Stable.Action.V3)
    (Stable.Reaction.V13)

include Register_old_rpc
    (struct let version = 13 end)
    (Stable.Action.V3)
    (Stable.Reaction.V12)

include Register_old_rpc
    (struct let version = 12 end)
    (Stable.Action.V3)
    (Stable.Reaction.V11)

include Register_old_rpc
    (struct let version = 11 end)
    (Stable.Action.V3)
    (Stable.Reaction.V10)

include Register_old_rpc
    (struct let version = 10 end)
    (Stable.Action.V3)
    (Stable.Reaction.V9)

include Register_old_rpc
    (struct let version = 9 end)
    (Stable.Action.V3)
    (Stable.Reaction.V8)

include Register_old_rpc
    (struct let version = 8 end)
    (Stable.Action.V2)
    (Stable.Reaction.V8)

module Action = Stable.Action.Model

module Assigned = struct
  include Stable.Assigned.Model

  let has_review_lines t =
    t.user_is_reviewing
    && Review_or_commit.count (Line_count.to_review_column_shown t.line_count) > 0
  ;;

  let has_follow_lines t =
    t.user_is_reviewing
    && t.line_count.review.follow > 0
  ;;

  let has_catch_up_lines t =
    Line_count.Catch_up.total t.line_count.catch_up > 0
  ;;
end

module Feature_info = Stable.Feature_info. Model
module Num_crs      = Stable.Num_crs.      Model
module Reaction     = Stable.Reaction.     Model
module Rev_facts    = Stable.Rev_facts.    Model
