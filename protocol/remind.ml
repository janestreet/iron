module Stable = struct

  open! Import_stable

  module Feature = Feature.Stable

  module Action = struct
    module V1 = struct
      type active_user_set =
        | Some_active of User_name.V1.Set.t
        | All_active
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: active_user_set];
        [%expect {| 56f0170da65735b3601dfe43807251ec |}]
      ;;

      type t =
        { feature_path : Feature_path.V1.t
        ; users        : active_user_set
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 0b73e98c5e21a694042cac714428589b |}]
      ;;

      let to_model t = t
    end

    module Model = V1
  end

  module Reaction = struct
    module V10 = struct
      type t =
        { description                    : string
        ; line_count_by_user             : (User_name.V1.t * Line_count.V5.t) list
        ; users_with_review_session_in_progress : User_name.V1.Set.t Or_error.V1.t
        ; users_with_unclean_workspaces
          : Unclean_workspace_reason.V2.t User_name.V1.Map.t
        ; cr_summary                     : Cr_comment.Summary.V1.t
        ; users                          : User_name.V1.Set.t
        ; next_bookmark_update           : Next_bookmark_update.V1.t
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ea1596142dcd746acf42fd48218e1d70 |}]
      ;;

      let of_model (m : t) = m
    end

    module V9 = struct
      type t =
        { description                    : string
        ; line_count_by_user             : (User_name.V1.t * Line_count.V5.t) list
        ; users_with_review_session_in_progress : User_name.V1.Set.t Or_error.V1.t
        ; users_with_unclean_workspaces
          : Unclean_workspace_reason.V1.t User_name.V1.Map.t
        ; cr_summary                     : Cr_comment.Summary.V1.t
        ; users                          : User_name.V1.Set.t
        ; next_bookmark_update           : Next_bookmark_update.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 3787703f3a2baf0a7cdf69a020ff4110 |}]
      ;;

      open! Core
      open! Import

      let of_model m =
        let { V10.
              description
            ; line_count_by_user
            ; users_with_review_session_in_progress
            ; users_with_unclean_workspaces
            ; cr_summary
            ; users
            ; next_bookmark_update
            } = V10.of_model m in
        let users_with_unclean_workspaces =
          Map.map users_with_unclean_workspaces
            ~f:Unclean_workspace_reason.Stable.V1.of_v2
        in
        { description
        ; line_count_by_user
        ; users_with_review_session_in_progress
        ; users_with_unclean_workspaces
        ; cr_summary
        ; users
        ; next_bookmark_update
        }
      ;;
    end

    module V8 = struct
      type t =
        { description                    : string
        ; line_count_by_user             : (User_name.V1.t * Line_count.V4.t) list
        ; users_with_uncommitted_session : User_name.V1.Set.t Or_error.V1.t
        ; users_with_unclean_workspaces
          : Unclean_workspace_reason.V1.t User_name.V1.Map.t
        ; cr_summary                     : Cr_comment.Summary.V1.t
        ; users                          : User_name.V1.Set.t
        ; next_bookmark_update           : Next_bookmark_update.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 1c4a3070c5d4e48fe9c61f2c4df1f6dd |}]
      ;;

      open! Core
      open! Import

      let of_model m =
        let { V9.
              description
            ; line_count_by_user
            ; users_with_review_session_in_progress
            ; users_with_unclean_workspaces
            ; cr_summary
            ; users
            ; next_bookmark_update
            } = V9.of_model m in
        let line_count_by_user =
          List.map line_count_by_user ~f:(fun (user, line_count) ->
            user, Line_count.Stable.V4.of_v5 line_count)
        in
        let users_with_uncommitted_session = users_with_review_session_in_progress in
        { description
        ; line_count_by_user
        ; users_with_uncommitted_session
        ; users_with_unclean_workspaces
        ; cr_summary
        ; users
        ; next_bookmark_update
        }
      ;;
    end

    module V7 = struct
      type t =
        { description                    : string
        ; line_count_by_user             : (User_name.V1.t * Line_count.V3.t) list
        ; users_with_uncommitted_session : User_name.V1.Set.t Or_error.V1.t
        ; users_with_unclean_workspaces
          : Unclean_workspace_reason.V1.t User_name.V1.Map.t
        ; cr_summary                     : Cr_comment.Summary.V1.t
        ; users                          : User_name.V1.Set.t
        ; next_bookmark_update           : Next_bookmark_update.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 4a4defdbf868b655b7b70047f47dd8a7 |}]
      ;;

      let of_model m =
        let { V8.
              description
            ; line_count_by_user
            ; users_with_uncommitted_session
            ; users_with_unclean_workspaces
            ; cr_summary
            ; users
            ; next_bookmark_update
            } = V8.of_model m in
        let line_count_by_user =
          List.map line_count_by_user ~f:(fun (user, line_count) ->
            user, Line_count.V3.of_v4 line_count)
        in
        { description
        ; line_count_by_user
        ; users_with_uncommitted_session
        ; users_with_unclean_workspaces
        ; cr_summary
        ; users
        ; next_bookmark_update
        }
      ;;

    end

    module V6 = struct
      type t =
        { description        : string
        ; line_count_by_user : (User_name.V1.t * int) list
        ; cr_summary         : Cr_comment.Summary.V1.t
        ; users              : User_name.V1.Set.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 1ec78353f91f1bbe827839233ba6ae99 |}]
      ;;

      open! Core
      open! Import

      let of_model m =
        let { V7.
              description
            ; line_count_by_user
            ; cr_summary
            ; users
            ; _
            } = V7.of_model m in
        let line_count_by_user =
          List.map line_count_by_user ~f:(fun (user, line_count) ->
            user, Review_or_commit.count line_count.review
          )
        in
        { description
        ; line_count_by_user
        ; cr_summary
        ; users
        }
      ;;
    end

    module Model = V10
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "remind" end)
    (struct let version = 10 end)
    (Stable.Action.V1)
    (Stable.Reaction.V10)

include Register_old_rpc
    (struct let version = 9 end)
    (Stable.Action.V1)
    (Stable.Reaction.V9)

include Register_old_rpc
    (struct let version = 8 end)
    (Stable.Action.V1)
    (Stable.Reaction.V8)

include Register_old_rpc
    (struct let version = 7 end)
    (Stable.Action.V1)
    (Stable.Reaction.V7)

include Register_old_rpc
    (struct let version = 6 end)
    (Stable.Action.V1)
    (Stable.Reaction.V6)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
