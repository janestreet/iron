module Stable = struct

  open! Import_stable

  module Locked = struct
    module V2 = struct
      type t =
        { by           : User_name.V1.t
        ; reason       : string
        ; at           : Time.V1_round_trippable.t
        ; is_permanent : bool
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f12ae5e96cb57ba814429ac704fc166d |}]
      ;;
    end

    module V1 = struct
      type t =
        { by     : User_name.V1.t
        ; reason : string
        ; at     : Time.V1_round_trippable.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| d972db9a41d8eda0cf6af113bf845597 |}]
      ;;

      let of_v2 { V2.
                  by
                ; reason
                ; at
                ; is_permanent = _
                } =
        { by
        ; reason
        ; at
        }
      ;;
    end

    module Model = V2
  end

  module Sorted_by = struct

    module V2 = struct
      type t =
        [ `Name
        | `Release_time
        | `Release_time_decreasing
        ]
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| d2c571136d167ef95a2ca99b1a5ee80e |}]
      ;;
    end

    module V1 = struct
      type t = [ `Name | `Release_order ] [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8657555ed2892969e81c717ea5c480ca |}]
      ;;

      let to_v2 = function
        | `Name as name  -> name
        | `Release_order -> `Release_time
      ;;
    end

    module Model = V2
  end

  module V23 = struct
    type t =
      { feature_id                : Feature_id.V1.t
      ; feature_path              : Feature_path.V1.t
      ; rev_zero                  : Rev.V1.t
      ; whole_feature_followers   : User_name.V1.Set.t
      ; whole_feature_reviewers   : User_name.V1.Set.t
      ; owners                    : User_name.V1.t list
      ; base                      : Rev.V1.t
      ; base_facts                : Rev_facts.V1.t Or_pending.V1.t
      ; next_base_update          : Next_base_update.V1.t
      ; crs_are_enabled           : bool
      ; crs_shown_in_todo_only_for_users_reviewing  : bool
      ; xcrs_shown_in_todo_only_for_users_reviewing : bool
      ; next_bookmark_update      : Next_bookmark_update.V1.t
      ; has_bookmark              : bool
      ; tip                       : Rev.V1.t
      ; tip_facts                 : Rev_facts.V1.t Or_pending.V1.t
      ; base_is_ancestor_of_tip   : Rev_facts.Is_ancestor.V1.t Or_pending.V1.t
      ; diff_from_base_to_tip     : Diff2s.V2.t Or_error.V1.t Or_pending.V1.t
      ; description               : string
      ; is_permanent              : bool
      ; seconder                  : User_name.V1.t option
      ; review_is_enabled         : bool
      ; reviewing                 : Reviewing.V1.t
      ; allow_review_for          : Allow_review_for.V1.t
      ; included_features         : Released_feature.V3.t list
      ; properties                : Properties.V1.t
      ; remote_repo_path          : Remote_repo_path.V1.t
      ; has_children              : bool
      ; release_process           : Release_process.V1.t
      ; who_can_release_into_me   : Who_can_release_into_me.V1.t
      ; send_email_to             : Email_address.V1.Set.t
      ; send_email_upon           : Send_email_upon.V1.Set.t
      ; locked                    : (Lock_name.V3.t * Locked.V2.t list) list
      ; line_count_by_user        : (User_name.V1.t * Line_count.V5.t) list Or_error.V1.t
      ; cr_summary                : Cr_comment.Summary.V1.t Or_error.V1.t
      ; next_steps                : Next_step.V6.t list
      ; users_with_review_session_in_progress : User_name.V1.Set.t Or_error.V1.t
      ; users_with_unclean_workspaces : Unclean_workspace_reason.V2.t User_name.V1.Map.t
      ; is_archived               : Is_archived.V1.t
      ; is_rebased                : bool
      ; latest_release            : Latest_release.V1.t option
      ; inheritable_attributes    : Inheritable_attributes.V1.t
      ; compilation_status        : Compilation_status.V1.t
      }

    [@@deriving bin_io, fields, sexp_of]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| bbb40131c73d97a42d227733fb81d5ff |}]
    ;;

    let of_model (m : t) = m
  end

  module V22 = struct
    type t =
      { feature_id                : Feature_id.V1.t
      ; feature_path              : Feature_path.V1.t
      ; rev_zero                  : Rev.V1.t
      ; whole_feature_followers   : User_name.V1.Set.t
      ; whole_feature_reviewers   : User_name.V1.Set.t
      ; owners                    : User_name.V1.t list
      ; base                      : Rev.V1.t
      ; base_facts                : Rev_facts.V1.t Or_pending.V1.t
      ; next_base_update          : Next_base_update.V1.t
      ; crs_are_enabled           : bool
      ; crs_shown_in_todo_only_for_users_reviewing  : bool
      ; xcrs_shown_in_todo_only_for_users_reviewing : bool
      ; next_bookmark_update      : Next_bookmark_update.V1.t
      ; has_bookmark              : bool
      ; tip                       : Rev.V1.t
      ; tip_facts                 : Rev_facts.V1.t Or_pending.V1.t
      ; base_is_ancestor_of_tip   : Rev_facts.Is_ancestor.V1.t Or_pending.V1.t
      ; diff_from_base_to_tip     : Diff2s.V2.t Or_error.V1.t Or_pending.V1.t
      ; description               : string
      ; is_permanent              : bool
      ; seconder                  : User_name.V1.t option
      ; review_is_enabled         : bool
      ; reviewing                 : Reviewing.V1.t
      ; allow_review_for          : Allow_review_for.V1.t
      ; included_features         : Released_feature.V3.t list
      ; properties                : Properties.V1.t
      ; remote_repo_path          : Remote_repo_path.V1.t
      ; has_children              : bool
      ; release_process           : Release_process.V1.t
      ; who_can_release_into_me   : Who_can_release_into_me.V1.t
      ; send_email_to             : Email_address.V1.Set.t
      ; send_email_upon           : Send_email_upon.V1.Set.t
      ; locked                    : (Lock_name.V3.t * Locked.V2.t list) list
      ; line_count_by_user        : (User_name.V1.t * Line_count.V5.t) list Or_error.V1.t
      ; cr_summary                : Cr_comment.Summary.V1.t Or_error.V1.t
      ; next_steps                : Next_step.V6.t list
      ; users_with_review_session_in_progress : User_name.V1.Set.t Or_error.V1.t
      ; users_with_unclean_workspaces : Unclean_workspace_reason.V2.t User_name.V1.Map.t
      ; is_archived               : bool
      ; is_rebased                : bool
      ; latest_release            : Latest_release.V1.t option
      ; inheritable_attributes    : Inheritable_attributes.V1.t
      }

    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 935878f048deb886d828a35d84977105 |}]
    ;;

    let of_model m =
      let { V23.
            feature_id
          ; feature_path
          ; rev_zero
          ; whole_feature_followers
          ; whole_feature_reviewers
          ; owners
          ; base
          ; base_facts
          ; next_base_update
          ; crs_are_enabled
          ; crs_shown_in_todo_only_for_users_reviewing
          ; xcrs_shown_in_todo_only_for_users_reviewing
          ; next_bookmark_update
          ; has_bookmark
          ; tip
          ; tip_facts
          ; base_is_ancestor_of_tip
          ; diff_from_base_to_tip
          ; description
          ; is_permanent
          ; seconder
          ; review_is_enabled
          ; reviewing
          ; allow_review_for
          ; included_features
          ; properties
          ; remote_repo_path
          ; has_children
          ; release_process
          ; who_can_release_into_me
          ; send_email_to
          ; send_email_upon
          ; locked
          ; line_count_by_user
          ; cr_summary
          ; next_steps
          ; users_with_review_session_in_progress
          ; users_with_unclean_workspaces
          ; is_archived
          ; is_rebased
          ; latest_release
          ; inheritable_attributes
          ; compilation_status = _
          } = V23.of_model m
      in
      { feature_id
      ; feature_path
      ; rev_zero
      ; whole_feature_followers
      ; whole_feature_reviewers
      ; owners
      ; base
      ; base_facts
      ; next_base_update
      ; crs_are_enabled
      ; crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; next_bookmark_update
      ; has_bookmark
      ; tip
      ; tip_facts
      ; base_is_ancestor_of_tip
      ; diff_from_base_to_tip
      ; description
      ; is_permanent
      ; seconder
      ; review_is_enabled
      ; reviewing
      ; allow_review_for
      ; included_features
      ; properties
      ; remote_repo_path
      ; has_children
      ; release_process
      ; who_can_release_into_me
      ; send_email_to
      ; send_email_upon
      ; locked
      ; line_count_by_user
      ; cr_summary
      ; next_steps
      ; users_with_review_session_in_progress
      ; users_with_unclean_workspaces
      ; is_archived = Is_archived.V1.to_bool is_archived
      ; is_rebased
      ; latest_release
      ; inheritable_attributes
      }
    ;;
  end

  module V21 = struct
    type t =
      { feature_id                : Feature_id.V1.t
      ; feature_path              : Feature_path.V1.t
      ; rev_zero                  : Rev.V1.t
      ; whole_feature_followers   : User_name.V1.Set.t
      ; whole_feature_reviewers   : User_name.V1.Set.t
      ; owners                    : User_name.V1.t list
      ; base                      : Rev.V1.t
      ; base_facts                : Rev_facts.V1.t Or_pending.V1.t
      ; next_base_update          : Next_base_update.V1.t
      ; crs_are_enabled           : bool
      ; crs_shown_in_todo_only_for_users_reviewing  : bool
      ; xcrs_shown_in_todo_only_for_users_reviewing : bool
      ; next_bookmark_update      : Next_bookmark_update.V1.t
      ; has_bookmark              : bool
      ; tip                       : Rev.V1.t
      ; tip_facts                 : Rev_facts.V1.t Or_pending.V1.t
      ; base_is_ancestor_of_tip   : Rev_facts.Is_ancestor.V1.t Or_pending.V1.t
      ; diff_from_base_to_tip     : Diff2s.V2.t Or_error.V1.t Or_pending.V1.t
      ; description               : string
      ; is_permanent              : bool
      ; seconder                  : User_name.V1.t option
      ; review_is_enabled         : bool
      ; reviewing                 : Reviewing.V1.t
      ; allow_review_for          : Allow_review_for.V1.t
      ; included_features         : Released_feature.V3.t list
      ; properties                : Properties.V1.t
      ; remote_repo_path          : Remote_repo_path.V1.t
      ; has_children              : bool
      ; release_process           : Release_process.V1.t
      ; who_can_release_into_me   : Who_can_release_into_me.V1.t
      ; send_email_to             : Email_address.V1.Set.t
      ; send_email_upon           : Send_email_upon.V1.Set.t
      ; locked                    : (Lock_name.V3.t * Locked.V2.t list) list
      ; line_count_by_user        : (User_name.V1.t * Line_count.V5.t) list Or_error.V1.t
      ; cr_summary                : Cr_comment.Summary.V1.t Or_error.V1.t
      ; next_steps                : Next_step.V6.t list
      ; users_with_review_session_in_progress : User_name.V1.Set.t Or_error.V1.t
      ; users_with_unclean_workspaces : Unclean_workspace_reason.V2.t User_name.V1.Map.t
      ; is_archived               : bool
      ; latest_release            : Latest_release.V1.t option
      ; inheritable_attributes    : Inheritable_attributes.V1.t
      }
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 41d1aeed9a63fcb8622eda64c7686de8 |}]
    ;;

    let of_model m =
      let { V22.
            feature_id
          ; feature_path
          ; rev_zero
          ; whole_feature_followers
          ; whole_feature_reviewers
          ; owners
          ; base
          ; base_facts
          ; next_base_update
          ; crs_are_enabled
          ; crs_shown_in_todo_only_for_users_reviewing
          ; xcrs_shown_in_todo_only_for_users_reviewing
          ; next_bookmark_update
          ; has_bookmark
          ; tip
          ; tip_facts
          ; base_is_ancestor_of_tip
          ; diff_from_base_to_tip
          ; description
          ; is_permanent
          ; seconder
          ; review_is_enabled
          ; reviewing
          ; allow_review_for
          ; included_features
          ; properties
          ; remote_repo_path
          ; has_children
          ; release_process
          ; who_can_release_into_me
          ; send_email_to
          ; send_email_upon
          ; locked
          ; line_count_by_user
          ; cr_summary
          ; next_steps
          ; users_with_review_session_in_progress
          ; users_with_unclean_workspaces
          ; is_archived
          ; is_rebased = _
          ; latest_release
          ; inheritable_attributes
          } = V22.of_model m
      in
      { feature_id
      ; feature_path
      ; rev_zero
      ; whole_feature_followers
      ; whole_feature_reviewers
      ; owners
      ; base
      ; base_facts
      ; next_base_update
      ; crs_are_enabled
      ; crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; next_bookmark_update
      ; has_bookmark
      ; tip
      ; tip_facts
      ; base_is_ancestor_of_tip
      ; diff_from_base_to_tip
      ; description
      ; is_permanent
      ; seconder
      ; review_is_enabled
      ; reviewing
      ; allow_review_for
      ; included_features
      ; properties
      ; remote_repo_path
      ; has_children
      ; release_process
      ; who_can_release_into_me
      ; send_email_to
      ; send_email_upon
      ; locked
      ; line_count_by_user
      ; cr_summary
      ; next_steps
      ; users_with_review_session_in_progress
      ; users_with_unclean_workspaces
      ; is_archived
      ; latest_release
      ; inheritable_attributes
      }
    ;;

  end

  module V20 = struct
    type t =
      { feature_id                : Feature_id.V1.t
      ; feature_path              : Feature_path.V1.t
      ; rev_zero                  : Rev.V1.t
      ; whole_feature_followers   : User_name.V1.Set.t
      ; whole_feature_reviewers   : User_name.V1.Set.t
      ; owners                    : User_name.V1.t list
      ; base                      : Rev.V1.t
      ; base_facts                : Rev_facts.V1.t Or_pending.V1.t
      ; next_base_update          : Next_base_update.V1.t
      ; crs_are_enabled           : bool
      ; crs_shown_in_todo_only_for_users_reviewing  : bool
      ; xcrs_shown_in_todo_only_for_users_reviewing : bool
      ; next_bookmark_update      : Next_bookmark_update.V1.t
      ; has_bookmark              : bool
      ; tip                       : Rev.V1.t
      ; tip_facts                 : Rev_facts.V1.t Or_pending.V1.t
      ; base_is_ancestor_of_tip   : Rev_facts.Is_ancestor.V1.t Or_pending.V1.t
      ; diff_from_base_to_tip     : Diff2s.V2.t Or_error.V1.t Or_pending.V1.t
      ; description               : string
      ; is_permanent              : bool
      ; seconder                  : User_name.V1.t option
      ; review_is_enabled         : bool
      ; reviewing                 : Reviewing.V1.t
      ; allow_review_for          : Allow_review_for.V1.t
      ; included_features         : Released_feature.V3.t list
      ; properties                : Properties.V1.t
      ; remote_repo_path          : Remote_repo_path.V1.t
      ; has_children              : bool
      ; release_process           : Release_process.V1.t
      ; who_can_release_into_me   : Who_can_release_into_me.V1.t
      ; send_email_to             : Email_address.V1.Set.t
      ; send_email_upon           : Send_email_upon.V1.Set.t
      ; locked                    : (Lock_name.V2.t * Locked.V2.t list) list
      ; line_count_by_user        : (User_name.V1.t * Line_count.V5.t) list Or_error.V1.t
      ; cr_summary                : Cr_comment.Summary.V1.t Or_error.V1.t
      ; next_steps                : Next_step.V5.t list
      ; users_with_review_session_in_progress : User_name.V1.Set.t Or_error.V1.t
      ; users_with_unclean_workspaces  : Unclean_workspace_reason.V1.t User_name.V1.Map.t
      ; is_archived               : bool
      ; latest_release            : Latest_release.V1.t option
      ; inheritable_attributes    : Inheritable_attributes.V1.t
      }
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| e2f1270ad04caf2d755873e12cb31fb9 |}]
    ;;

    open! Core
    open! Import

    let of_model m =
      let { V21.
            feature_id
          ; feature_path
          ; rev_zero
          ; whole_feature_followers
          ; whole_feature_reviewers
          ; owners
          ; base
          ; base_facts
          ; next_base_update
          ; crs_are_enabled
          ; crs_shown_in_todo_only_for_users_reviewing
          ; xcrs_shown_in_todo_only_for_users_reviewing
          ; next_bookmark_update
          ; has_bookmark
          ; tip
          ; tip_facts
          ; base_is_ancestor_of_tip
          ; diff_from_base_to_tip
          ; description
          ; is_permanent
          ; seconder
          ; review_is_enabled
          ; reviewing
          ; allow_review_for
          ; included_features
          ; properties
          ; remote_repo_path
          ; has_children
          ; release_process
          ; who_can_release_into_me
          ; locked
          ; line_count_by_user
          ; cr_summary
          ; next_steps
          ; users_with_review_session_in_progress
          ; users_with_unclean_workspaces
          ; is_archived
          ; latest_release
          ; inheritable_attributes
          ; send_email_to
          ; send_email_upon
          } = V21.of_model m
      in
      let locked = List.filter_map locked ~f:(fun (lock_name, locked) ->
        Option.map (Lock_name.Stable.V2.of_v3 lock_name)
          ~f:(fun lock_name -> lock_name, locked))
      in
      let next_steps = List.map next_steps ~f:Next_step.Stable.V5.of_v6 in
      let users_with_unclean_workspaces =
        Map.map users_with_unclean_workspaces
          ~f:Unclean_workspace_reason.Stable.V1.of_v2
      in
      { feature_id
      ; feature_path
      ; rev_zero
      ; whole_feature_followers
      ; whole_feature_reviewers
      ; owners
      ; base
      ; base_facts
      ; next_base_update
      ; crs_are_enabled
      ; crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; next_bookmark_update
      ; has_bookmark
      ; tip
      ; tip_facts
      ; base_is_ancestor_of_tip
      ; diff_from_base_to_tip
      ; description
      ; is_permanent
      ; seconder
      ; review_is_enabled
      ; reviewing
      ; allow_review_for
      ; included_features
      ; properties
      ; remote_repo_path
      ; has_children
      ; release_process
      ; who_can_release_into_me
      ; send_email_to
      ; send_email_upon
      ; locked
      ; line_count_by_user
      ; cr_summary
      ; next_steps
      ; users_with_review_session_in_progress
      ; users_with_unclean_workspaces
      ; is_archived
      ; latest_release
      ; inheritable_attributes
      }
    ;;
  end

  module V19 = struct
    type t =
      { feature_id                : Feature_id.V1.t
      ; feature_path              : Feature_path.V1.t
      ; rev_zero                  : Rev.V1.t
      ; whole_feature_followers   : User_name.V1.Set.t
      ; whole_feature_reviewers   : User_name.V1.Set.t
      ; owners                    : User_name.V1.t list
      ; base                      : Rev.V1.t
      ; base_facts                : Rev_facts.V1.t Or_pending.V1.t
      ; next_base_update          : Next_base_update.V1.t
      ; crs_are_enabled           : bool
      ; crs_shown_in_todo_only_for_users_reviewing  : bool
      ; xcrs_shown_in_todo_only_for_users_reviewing : bool
      ; next_bookmark_update      : Next_bookmark_update.V1.t
      ; has_bookmark              : bool
      ; tip                       : Rev.V1.t
      ; tip_facts                 : Rev_facts.V1.t Or_pending.V1.t
      ; base_is_ancestor_of_tip   : Rev_facts.Is_ancestor.V1.t Or_pending.V1.t
      ; diff_from_base_to_tip     : Diff2s.V2.t Or_error.V1.t Or_pending.V1.t
      ; description               : string
      ; is_permanent              : bool
      ; seconder                  : User_name.V1.t option
      ; review_is_enabled         : bool
      ; reviewing                 : Reviewing.V1.t
      ; allow_review_for          : Allow_review_for.V1.t
      ; included_features         : Released_feature.V3.t list
      ; properties                : Properties.V1.t
      ; remote_repo_path          : Remote_repo_path.V1.t
      ; has_children              : bool
      ; release_process           : Release_process.V1.t
      ; who_can_release_into_me   : Who_can_release_into_me.V1.t
      ; send_email_to             : Email_address.V1.Set.t
      ; send_email_upon           : Send_email_upon.V1.Set.t
      ; locked                    : (Lock_name.V2.t * Locked.V2.t list) list
      ; line_count_by_user        : (User_name.V1.t * Line_count.V4.t) list Or_error.V1.t
      ; cr_summary                : Cr_comment.Summary.V1.t Or_error.V1.t
      ; next_steps                : Next_step.V5.t list
      ; users_with_uncommitted_session : User_name.V1.Set.t Or_error.V1.t
      ; users_with_unclean_workspaces  : Unclean_workspace_reason.V1.t User_name.V1.Map.t
      ; is_archived               : bool
      ; latest_release            : Latest_release.V1.t option
      }
    [@@deriving bin_io]


    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| f446db0d717c71116bd10fff3c797e8a |}]
    ;;
    open! Core
    open! Import

    let of_model m =
      let { V20.
            feature_id
          ; feature_path
          ; rev_zero
          ; whole_feature_followers
          ; whole_feature_reviewers
          ; owners
          ; base
          ; base_facts
          ; next_base_update
          ; crs_are_enabled
          ; crs_shown_in_todo_only_for_users_reviewing
          ; xcrs_shown_in_todo_only_for_users_reviewing
          ; next_bookmark_update
          ; has_bookmark
          ; tip
          ; tip_facts
          ; base_is_ancestor_of_tip
          ; diff_from_base_to_tip
          ; description
          ; is_permanent
          ; seconder
          ; review_is_enabled
          ; reviewing
          ; allow_review_for
          ; included_features
          ; properties
          ; remote_repo_path
          ; has_children
          ; release_process
          ; who_can_release_into_me
          ; send_email_to
          ; send_email_upon
          ; locked
          ; line_count_by_user
          ; cr_summary
          ; next_steps
          ; users_with_review_session_in_progress
          ; users_with_unclean_workspaces
          ; is_archived
          ; latest_release
          ; inheritable_attributes = _
          } = V20.of_model m in
      let line_count_by_user =
        line_count_by_user
        |> Or_error.map ~f:(List.map ~f:(fun (user, line_count) ->
          user, Line_count.Stable.V4.of_v5 line_count))
      in
      let users_with_uncommitted_session = users_with_review_session_in_progress in
      { feature_id
      ; feature_path
      ; rev_zero
      ; whole_feature_followers
      ; whole_feature_reviewers
      ; owners
      ; base
      ; base_facts
      ; next_base_update
      ; crs_are_enabled
      ; crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; next_bookmark_update
      ; has_bookmark
      ; tip
      ; tip_facts
      ; base_is_ancestor_of_tip
      ; diff_from_base_to_tip
      ; description
      ; is_permanent
      ; seconder
      ; review_is_enabled
      ; reviewing
      ; allow_review_for
      ; included_features
      ; properties
      ; remote_repo_path
      ; has_children
      ; release_process
      ; who_can_release_into_me
      ; send_email_to
      ; send_email_upon
      ; locked
      ; line_count_by_user
      ; cr_summary
      ; next_steps
      ; users_with_uncommitted_session
      ; users_with_unclean_workspaces
      ; is_archived
      ; latest_release
      }
    ;;
  end

  module V18 = struct
    type t =
      { feature_id                : Feature_id.V1.t
      ; feature_path              : Feature_path.V1.t
      ; rev_zero                  : Rev.V1.t
      ; whole_feature_followers   : User_name.V1.Set.t
      ; whole_feature_reviewers   : User_name.V1.Set.t
      ; owners                    : User_name.V1.t list
      ; base                      : Rev.V1.t
      ; base_facts                : Rev_facts.V1.t Or_pending.V1.t
      ; next_base_update          : Next_base_update.V1.t
      ; crs_are_enabled           : bool
      ; crs_shown_in_todo_only_for_users_reviewing  : bool
      ; xcrs_shown_in_todo_only_for_users_reviewing : bool
      ; next_bookmark_update      : Next_bookmark_update.V1.t
      ; has_bookmark              : bool
      ; tip                       : Rev.V1.t
      ; tip_facts                 : Rev_facts.V1.t Or_pending.V1.t
      ; base_is_ancestor_of_tip   : Rev_facts.Is_ancestor.V1.t Or_pending.V1.t
      ; diff_from_base_to_tip     : Diff2s.V2.t Or_error.V1.t Or_pending.V1.t
      ; description               : string
      ; is_permanent              : bool
      ; seconder                  : User_name.V1.t option
      ; review_is_enabled         : bool
      ; reviewing                 : Reviewing.V1.t
      ; allow_review_for          : Allow_review_for.V1.t
      ; included_features         : Released_feature.V3.t list
      ; properties                : Properties.V1.t
      ; remote_repo_path          : Remote_repo_path.V1.t
      ; has_children              : bool
      ; release_process           : Release_process.V1.t
      ; who_can_release_into_me   : Who_can_release_into_me.V1.t
      ; send_email_to             : Email_address.V1.Set.t
      ; send_email_upon           : Send_email_upon.V1.Set.t
      ; locked                    : (Lock_name.V2.t * Locked.V2.t list) list
      ; line_count_by_user        : (User_name.V1.t * Line_count.V3.t) list Or_error.V1.t
      ; cr_summary                : Cr_comment.Summary.V1.t Or_error.V1.t
      ; next_steps                : Next_step.V5.t list
      ; users_with_uncommitted_session : User_name.V1.Set.t Or_error.V1.t
      ; users_with_unclean_workspaces  : Unclean_workspace_reason.V1.t User_name.V1.Map.t
      ; is_archived               : bool
      ; latest_release            : Latest_release.V1.t option
      }
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| e7793dae462a87da88774894eb599e89 |}]
    ;;

    open! Core
    open! Import

    let of_model m =
      let { V19.
            feature_id
          ; feature_path
          ; rev_zero
          ; whole_feature_followers
          ; whole_feature_reviewers
          ; owners
          ; base
          ; base_facts
          ; next_base_update
          ; crs_are_enabled
          ; crs_shown_in_todo_only_for_users_reviewing
          ; xcrs_shown_in_todo_only_for_users_reviewing
          ; next_bookmark_update
          ; has_bookmark
          ; tip
          ; tip_facts
          ; base_is_ancestor_of_tip
          ; diff_from_base_to_tip
          ; description
          ; is_permanent
          ; seconder
          ; review_is_enabled
          ; reviewing
          ; allow_review_for
          ; included_features
          ; properties
          ; remote_repo_path
          ; has_children
          ; release_process
          ; who_can_release_into_me
          ; send_email_to
          ; send_email_upon
          ; locked
          ; line_count_by_user
          ; cr_summary
          ; next_steps
          ; users_with_uncommitted_session
          ; users_with_unclean_workspaces
          ; is_archived
          ; latest_release
          } = V19.of_model m in
      let line_count_by_user =
        line_count_by_user
        |> Or_error.map ~f:(List.filter_map ~f:(fun (user, line_count) ->
          let { Line_count.Stable.V3. review; follow; catch_up; completed } as line_count
            = Line_count.Stable.V3.of_v4 line_count
          in
          if Review_or_commit.count review > 0
          || follow > 0
          || catch_up > 0
          || completed > 0
          then Some (user, line_count)
          else None))
      in
      { feature_id
      ; feature_path
      ; rev_zero
      ; whole_feature_followers
      ; whole_feature_reviewers
      ; owners
      ; base
      ; base_facts
      ; next_base_update
      ; crs_are_enabled
      ; crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; next_bookmark_update
      ; has_bookmark
      ; tip
      ; tip_facts
      ; base_is_ancestor_of_tip
      ; diff_from_base_to_tip
      ; description
      ; is_permanent
      ; seconder
      ; review_is_enabled
      ; reviewing
      ; allow_review_for
      ; included_features
      ; properties
      ; remote_repo_path
      ; has_children
      ; release_process
      ; who_can_release_into_me
      ; send_email_to
      ; send_email_upon
      ; locked
      ; line_count_by_user
      ; cr_summary
      ; next_steps
      ; users_with_uncommitted_session
      ; users_with_unclean_workspaces
      ; is_archived
      ; latest_release
      }
    ;;
  end

  module V17 = struct
    type t =
      { feature_id                : Feature_id.V1.t
      ; feature_path              : Feature_path.V1.t
      ; rev_zero                  : Rev.V1.t
      ; whole_feature_followers   : User_name.V1.Set.t
      ; whole_feature_reviewers   : User_name.V1.Set.t
      ; owners                    : User_name.V1.t list
      ; base                      : Rev.V1.t
      ; base_facts                : Rev_facts.V1.t Or_pending.V1.t
      ; crs_are_enabled           : bool
      ; crs_shown_in_todo_only_for_users_reviewing  : bool
      ; xcrs_shown_in_todo_only_for_users_reviewing : bool
      ; next_bookmark_update      : Next_bookmark_update.V1.t
      ; has_bookmark              : bool
      ; tip                       : Rev.V1.t
      ; tip_facts                 : Rev_facts.V1.t Or_pending.V1.t
      ; base_is_ancestor_of_tip   : Rev_facts.Is_ancestor.V1.t Or_pending.V1.t
      ; diff_from_base_to_tip     : Diff2s.V2.t Or_error.V1.t Or_pending.V1.t
      ; description               : string
      ; is_permanent              : bool
      ; seconder                  : User_name.V1.t option
      ; review_is_enabled         : bool
      ; reviewing                 : Reviewing.V1.t
      ; allow_review_for          : Allow_review_for.V1.t
      ; included_features         : Released_feature.V3.t list
      ; properties                : Properties.V1.t
      ; remote_repo_path          : Remote_repo_path.V1.t
      ; has_children              : bool
      ; release_process           : Release_process.V1.t
      ; who_can_release_into_me   : Who_can_release_into_me.V1.t
      ; send_email_to             : Email_address.V1.Set.t
      ; send_email_upon           : Send_email_upon.V1.Set.t
      ; locked                    : (Lock_name.V2.t * Locked.V2.t list) list
      ; line_count_by_user        : (User_name.V1.t * Line_count.V3.t) list Or_error.V1.t
      ; cr_summary                : Cr_comment.Summary.V1.t Or_error.V1.t
      ; next_steps                : Next_step.V5.t list
      ; users_with_uncommitted_session : User_name.V1.Set.t Or_error.V1.t
      ; users_with_unclean_workspaces  : Unclean_workspace_reason.V1.t User_name.V1.Map.t
      ; is_archived               : bool
      ; latest_release            : Latest_release.V1.t option
      }
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| d78b4e27e549d7f69cee3baff15bfe99 |}]
    ;;

    let of_model m =
      let { V18.
            feature_id
          ; feature_path
          ; rev_zero
          ; whole_feature_followers
          ; whole_feature_reviewers
          ; owners
          ; base
          ; base_facts
          ; next_base_update = _
          ; crs_are_enabled
          ; crs_shown_in_todo_only_for_users_reviewing
          ; xcrs_shown_in_todo_only_for_users_reviewing
          ; next_bookmark_update
          ; has_bookmark
          ; tip
          ; tip_facts
          ; base_is_ancestor_of_tip
          ; diff_from_base_to_tip
          ; description
          ; is_permanent
          ; seconder
          ; review_is_enabled
          ; reviewing
          ; allow_review_for
          ; included_features
          ; properties
          ; remote_repo_path
          ; has_children
          ; release_process
          ; who_can_release_into_me
          ; send_email_to
          ; send_email_upon
          ; locked
          ; line_count_by_user
          ; cr_summary
          ; next_steps
          ; users_with_uncommitted_session
          ; users_with_unclean_workspaces
          ; is_archived
          ; latest_release
          } = V18.of_model m in
      { feature_id
      ; feature_path
      ; rev_zero
      ; whole_feature_followers
      ; whole_feature_reviewers
      ; owners
      ; base
      ; base_facts
      ; crs_are_enabled
      ; crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; next_bookmark_update
      ; has_bookmark
      ; tip
      ; tip_facts
      ; base_is_ancestor_of_tip
      ; diff_from_base_to_tip
      ; description
      ; is_permanent
      ; seconder
      ; review_is_enabled
      ; reviewing
      ; allow_review_for
      ; included_features
      ; properties
      ; remote_repo_path
      ; has_children
      ; release_process
      ; who_can_release_into_me
      ; send_email_to
      ; send_email_upon
      ; locked
      ; line_count_by_user
      ; cr_summary
      ; next_steps
      ; users_with_uncommitted_session
      ; users_with_unclean_workspaces
      ; is_archived
      ; latest_release
      }
    ;;
  end

  module V16 = struct
    type t =
      { feature_id                : Feature_id.V1.t
      ; feature_path              : Feature_path.V1.t
      ; rev_zero                  : Rev.V1.t
      ; whole_feature_followers   : User_name.V1.Set.t
      ; whole_feature_reviewers   : User_name.V1.Set.t
      ; owners                    : User_name.V1.t list
      ; base                      : Rev.V1.t
      ; base_facts                : Rev_facts.V1.t Or_pending.V1.t
      ; crs_are_enabled           : bool
      ; crs_shown_in_todo_only_for_users_reviewing  : bool
      ; xcrs_shown_in_todo_only_for_users_reviewing : bool
      ; next_bookmark_update      : Next_bookmark_update.V1.t
      ; has_bookmark              : bool
      ; tip                       : Rev.V1.t
      ; tip_facts                 : Rev_facts.V1.t Or_pending.V1.t
      ; base_is_ancestor_of_tip   : Rev_facts.Is_ancestor.V1.t Or_pending.V1.t
      ; diff_from_base_to_tip     : Diff2s.V2.t Or_error.V1.t Or_pending.V1.t
      ; description               : string
      ; is_permanent              : bool
      ; seconder                  : User_name.V1.t option
      ; review_is_enabled         : bool
      ; reviewing                 : Reviewing.V1.t
      ; allow_review_for          : Allow_review_for.V1.t
      ; included_features         : Released_feature.V3.t list
      ; properties                : Properties.V1.t
      ; remote_repo_path          : Remote_repo_path.V1.t
      ; has_children              : bool
      ; release_process           : Release_process.V1.t
      ; who_can_release_into_me   : Who_can_release_into_me.V1.t
      ; send_email_to             : Email_address.V1.Set.t
      ; send_email_upon           : Send_email_upon.V1.Set.t
      ; locked                    : (Lock_name.V1.t * Locked.V2.t list) list
      ; line_count_by_user        : (User_name.V1.t * Line_count.V3.t) list Or_error.V1.t
      ; cr_summary                : Cr_comment.Summary.V1.t Or_error.V1.t
      ; next_steps                : Next_step.V5.t list
      ; users_with_uncommitted_session : User_name.V1.Set.t Or_error.V1.t
      ; is_archived               : bool
      ; latest_release            : Latest_release.V1.t option
      }
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 0eb5b62f1188654f48ddca51ea89a3e0 |}]
    ;;

    open! Core
    open! Import

    let of_model m =
      let { V17.
            feature_id
          ; feature_path
          ; rev_zero
          ; whole_feature_followers
          ; whole_feature_reviewers
          ; owners
          ; base
          ; base_facts
          ; crs_are_enabled
          ; crs_shown_in_todo_only_for_users_reviewing
          ; xcrs_shown_in_todo_only_for_users_reviewing
          ; next_bookmark_update
          ; has_bookmark
          ; tip
          ; tip_facts
          ; base_is_ancestor_of_tip
          ; diff_from_base_to_tip
          ; description
          ; is_permanent
          ; seconder
          ; review_is_enabled
          ; reviewing
          ; allow_review_for
          ; included_features
          ; properties
          ; remote_repo_path
          ; has_children
          ; release_process
          ; who_can_release_into_me
          ; send_email_to
          ; send_email_upon
          ; locked
          ; line_count_by_user
          ; cr_summary
          ; next_steps
          ; users_with_uncommitted_session
          ; users_with_unclean_workspaces = _
          ; is_archived
          ; latest_release
          } = V17.of_model m in
      let locked =
        List.filter_map locked ~f:(fun (lock_name, locks) ->
          Option.map (Lock_name.Stable.V1.of_v2 lock_name) ~f:(fun lock -> lock, locks))
      in
      { feature_id
      ; feature_path
      ; rev_zero
      ; whole_feature_followers
      ; whole_feature_reviewers
      ; owners
      ; base
      ; base_facts
      ; crs_are_enabled
      ; crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; next_bookmark_update
      ; has_bookmark
      ; tip
      ; tip_facts
      ; base_is_ancestor_of_tip
      ; diff_from_base_to_tip
      ; description
      ; is_permanent
      ; seconder
      ; review_is_enabled
      ; reviewing
      ; allow_review_for
      ; included_features
      ; properties
      ; remote_repo_path
      ; has_children
      ; release_process
      ; who_can_release_into_me
      ; send_email_to
      ; send_email_upon
      ; locked
      ; line_count_by_user
      ; cr_summary
      ; next_steps
      ; users_with_uncommitted_session
      ; is_archived
      ; latest_release
      }
    ;;

  end

  module V15 = struct
    type t =
      { feature_id                : Feature_id.V1.t
      ; feature_path              : Feature_path.V1.t
      ; rev_zero                  : Rev.V1.t
      ; whole_feature_followers   : User_name.V1.Set.t
      ; whole_feature_reviewers   : User_name.V1.Set.t
      ; owners                    : User_name.V1.t list
      ; base                      : Rev.V1.t
      ; base_facts                : Rev_facts.V1.t Or_pending.V1.t
      ; crs_are_enabled           : bool
      ; crs_shown_in_todo_only_for_users_reviewing  : bool
      ; xcrs_shown_in_todo_only_for_users_reviewing : bool
      ; next_bookmark_update      : Next_bookmark_update.V1.t
      ; has_bookmark              : bool
      ; tip                       : Rev.V1.t
      ; tip_facts                 : Rev_facts.V1.t Or_pending.V1.t
      ; base_is_ancestor_of_tip   : Rev_facts.Is_ancestor.V1.t Or_pending.V1.t
      ; diff_from_base_to_tip     : Diff2s.V2.t Or_error.V1.t Or_pending.V1.t
      ; description               : string
      ; is_permanent              : bool
      ; seconder                  : User_name.V1.t option
      ; review_is_enabled         : bool
      ; reviewing                 : Reviewing.V1.t
      ; allow_review_for          : Allow_review_for.V1.t
      ; included_features         : Released_feature.V3.t list
      ; properties                : Properties.V1.t
      ; remote_repo_path          : Remote_repo_path.V1.t
      ; has_children              : bool
      ; release_process           : Release_process.V1.t
      ; who_can_release_into_me   : Who_can_release_into_me.V1.t
      ; send_email_to             : Email_address.V1.Set.t
      ; send_email_upon           : Send_email_upon.V1.Set.t
      ; locked                    : (Lock_name.V1.t * Locked.V1.t list) list
      ; line_count_by_user        : (User_name.V1.t * Line_count.V3.t) list Or_error.V1.t
      ; cr_summary                : Cr_comment.Summary.V1.t Or_error.V1.t
      ; next_steps                : Next_step.V5.t list
      ; users_with_uncommitted_session : User_name.V1.Set.t Or_error.V1.t
      ; is_archived               : bool
      ; latest_release            : Latest_release.V1.t option
      }
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| bd9a497903513fdb0d067a2ce6ec6f92 |}]
    ;;

    let of_model m =
      let { V16.
            feature_id
          ; feature_path
          ; rev_zero
          ; whole_feature_followers
          ; whole_feature_reviewers
          ; owners
          ; base
          ; base_facts
          ; crs_are_enabled
          ; crs_shown_in_todo_only_for_users_reviewing
          ; xcrs_shown_in_todo_only_for_users_reviewing
          ; next_bookmark_update
          ; has_bookmark
          ; tip
          ; tip_facts
          ; base_is_ancestor_of_tip
          ; diff_from_base_to_tip
          ; description
          ; is_permanent
          ; seconder
          ; review_is_enabled
          ; reviewing
          ; allow_review_for
          ; included_features
          ; properties
          ; remote_repo_path
          ; has_children
          ; release_process
          ; who_can_release_into_me
          ; send_email_to
          ; send_email_upon
          ; locked
          ; line_count_by_user
          ; cr_summary
          ; next_steps
          ; users_with_uncommitted_session
          ; is_archived
          ; latest_release
          } = V16.of_model m in
      let locked =
        List.map locked ~f:(fun (lock_name, locks) ->
          lock_name, List.map locks ~f:Locked.V1.of_v2)
      in
      { feature_id
      ; feature_path
      ; rev_zero
      ; whole_feature_followers
      ; whole_feature_reviewers
      ; owners
      ; base
      ; base_facts
      ; crs_are_enabled
      ; crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; next_bookmark_update
      ; has_bookmark
      ; tip
      ; tip_facts
      ; base_is_ancestor_of_tip
      ; diff_from_base_to_tip
      ; description
      ; is_permanent
      ; seconder
      ; review_is_enabled
      ; reviewing
      ; allow_review_for
      ; included_features
      ; properties
      ; remote_repo_path
      ; has_children
      ; release_process
      ; who_can_release_into_me
      ; send_email_to
      ; send_email_upon
      ; locked
      ; line_count_by_user
      ; cr_summary
      ; next_steps
      ; users_with_uncommitted_session
      ; is_archived
      ; latest_release
      }
    ;;
  end

  module V14 = struct
    type t =
      { feature_id                : Feature_id.V1.t
      ; feature_path              : Feature_path.V1.t
      ; rev_zero                  : Rev.V1.t
      ; whole_feature_followers   : User_name.V1.Set.t
      ; whole_feature_reviewers   : User_name.V1.Set.t
      ; owners                    : User_name.V1.t list
      ; base                      : Rev.V1.t
      ; base_facts                : Rev_facts.V1.t Or_pending.V1.t
      ; crs_are_enabled           : bool
      ; crs_shown_in_todo_only_for_users_reviewing  : bool
      ; xcrs_shown_in_todo_only_for_users_reviewing : bool
      ; next_bookmark_update      : Next_bookmark_update.V1.t
      ; has_bookmark              : bool
      ; tip                       : Rev.V1.t
      ; tip_facts                 : Rev_facts.V1.t Or_pending.V1.t
      ; base_is_ancestor_of_tip   : Rev_facts.Is_ancestor.V1.t Or_pending.V1.t
      ; description               : string
      ; is_permanent              : bool
      ; seconder                  : User_name.V1.t option
      ; review_is_enabled         : bool
      ; reviewing                 : Reviewing.V1.t
      ; allow_review_for          : Allow_review_for.V1.t
      ; included_features         : Released_feature.V3.t list
      ; properties                : Properties.V1.t
      ; remote_repo_path          : Remote_repo_path.V1.t
      ; has_children              : bool
      ; release_process           : Release_process.V1.t
      ; who_can_release_into_me   : Who_can_release_into_me.V1.t
      ; send_email_to             : Email_address.V1.Set.t
      ; send_email_upon           : Send_email_upon.V1.Set.t
      ; locked                    : (Lock_name.V1.t * Locked.V1.t list) list
      ; line_count_by_user        : (User_name.V1.t * Line_count.V3.t) list Or_error.V1.t
      ; cr_summary                : Cr_comment.Summary.V1.t Or_error.V1.t
      ; next_steps                : Next_step.V5.t list
      ; users_with_uncommitted_session : User_name.V1.Set.t Or_error.V1.t
      }
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 83579b302253056b2f2a434c2d2b1a37 |}]
    ;;

    let of_model m =
      let { V15.
            feature_id
          ; feature_path
          ; rev_zero
          ; whole_feature_followers
          ; whole_feature_reviewers
          ; owners
          ; base
          ; base_facts
          ; crs_are_enabled
          ; crs_shown_in_todo_only_for_users_reviewing
          ; xcrs_shown_in_todo_only_for_users_reviewing
          ; next_bookmark_update
          ; has_bookmark
          ; tip
          ; tip_facts
          ; base_is_ancestor_of_tip
          ; description
          ; is_permanent
          ; seconder
          ; review_is_enabled
          ; reviewing
          ; allow_review_for
          ; included_features
          ; properties
          ; remote_repo_path
          ; has_children
          ; release_process
          ; who_can_release_into_me
          ; send_email_to
          ; send_email_upon
          ; locked
          ; line_count_by_user
          ; cr_summary
          ; next_steps
          ; users_with_uncommitted_session
          ; _
          } = V15.of_model m in
      { feature_id
      ; feature_path
      ; rev_zero
      ; whole_feature_followers
      ; whole_feature_reviewers
      ; owners
      ; base
      ; base_facts
      ; crs_are_enabled
      ; crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; next_bookmark_update
      ; has_bookmark
      ; tip
      ; tip_facts
      ; base_is_ancestor_of_tip
      ; description
      ; is_permanent
      ; seconder
      ; review_is_enabled
      ; reviewing
      ; allow_review_for
      ; included_features
      ; properties
      ; remote_repo_path
      ; has_children
      ; release_process
      ; who_can_release_into_me
      ; send_email_to
      ; send_email_upon
      ; locked
      ; line_count_by_user
      ; cr_summary
      ; next_steps
      ; users_with_uncommitted_session
      }
    ;;
  end

  let line_count_by_user_of_v3 line_count_by_user =
    Or_error.V1.map line_count_by_user ~f:(fun by_user ->
      List.filter_map by_user ~f:(fun (user, line_count) ->
        let ({ Line_count.V2.
               review
             ; catch_up
             ; completed
             } as line_count) = Line_count.V2.of_v3 line_count in
        if review > 0 || catch_up > 0 || completed > 0
        then Some (user, line_count)
        else None))
  ;;

  module V13 = struct
    type t =
      { feature_id                : Feature_id.V1.t
      ; feature_path              : Feature_path.V1.t
      ; rev_zero                  : Rev.V1.t
      ; whole_feature_reviewers   : User_name.V1.Set.t
      ; owners                    : User_name.V1.t list
      ; base                      : Rev.V1.t
      ; base_facts                : Rev_facts.V1.t Or_pending.V1.t
      ; crs_are_enabled           : bool
      ; crs_shown_in_todo_only_for_users_reviewing  : bool
      ; xcrs_shown_in_todo_only_for_users_reviewing : bool
      ; next_bookmark_update      : Next_bookmark_update.V1.t
      ; has_bookmark              : bool
      ; tip                       : Rev.V1.t
      ; tip_facts                 : Rev_facts.V1.t Or_pending.V1.t
      ; base_is_ancestor_of_tip   : Rev_facts.Is_ancestor.V1.t Or_pending.V1.t
      ; description               : string
      ; is_permanent              : bool
      ; seconder                  : User_name.V1.t option
      ; review_is_enabled         : bool
      ; reviewing                 : Reviewing.V1.t
      ; allow_review_for          : Allow_review_for.V1.t
      ; included_features         : Released_feature.V2.t list
      ; properties                : Properties.V1.t
      ; remote_repo_path          : Remote_repo_path.V1.t
      ; has_children              : bool
      ; release_process           : Release_process.V1.t
      ; who_can_release_into_me   : Who_can_release_into_me.V1.t
      ; send_email_to             : Email_address.V1.Set.t
      ; send_email_upon           : Send_email_upon.V1.Set.t
      ; locked                    : (Lock_name.V1.t * Locked.V1.t list) list
      ; line_count_by_user        : (User_name.V1.t * Line_count.V2.t) list Or_error.V1.t
      ; cr_summary                : Cr_comment.Summary.V1.t Or_error.V1.t
      ; next_steps                : Next_step.V5.t list
      }
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 5f036347ab82677d08969dd2814bb0ab |}]
    ;;

    open! Core
    open! Import

    let of_model m =
      let { V14.
            feature_id
          ; feature_path
          ; rev_zero
          ; whole_feature_reviewers
          ; owners
          ; base
          ; base_facts
          ; crs_are_enabled
          ; crs_shown_in_todo_only_for_users_reviewing
          ; xcrs_shown_in_todo_only_for_users_reviewing
          ; next_bookmark_update
          ; has_bookmark
          ; tip
          ; tip_facts
          ; base_is_ancestor_of_tip
          ; description
          ; is_permanent
          ; seconder
          ; review_is_enabled
          ; reviewing
          ; allow_review_for
          ; included_features
          ; properties
          ; remote_repo_path
          ; has_children
          ; release_process
          ; who_can_release_into_me
          ; send_email_to
          ; send_email_upon
          ; locked
          ; line_count_by_user
          ; cr_summary
          ; next_steps
          ; _
          } = V14.of_model m in
      { feature_id
      ; feature_path
      ; rev_zero
      ; whole_feature_reviewers
      ; owners
      ; base
      ; base_facts
      ; crs_are_enabled
      ; crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; next_bookmark_update
      ; has_bookmark
      ; tip
      ; tip_facts
      ; base_is_ancestor_of_tip
      ; description
      ; is_permanent
      ; seconder
      ; review_is_enabled
      ; reviewing
      ; allow_review_for
      ; included_features
        = List.map included_features ~f:Released_feature.Stable.V2.of_v3
      ; properties
      ; remote_repo_path
      ; has_children
      ; release_process
      ; who_can_release_into_me
      ; send_email_to
      ; send_email_upon
      ; locked
      ; line_count_by_user = line_count_by_user_of_v3 line_count_by_user
      ; cr_summary
      ; next_steps
      }
    ;;
  end

  module V12 = struct
    type t =
      { feature_id                : Feature_id.V1.t
      ; feature_path              : Feature_path.V1.t
      ; rev_zero                  : Rev.V1.t
      ; whole_feature_reviewers   : User_name.V1.Set.t
      ; owners                    : User_name.V1.t list
      ; base                      : Rev.V1.t
      ; base_facts                : Rev_facts.V1.t Or_pending.V1.t
      ; crs_are_enabled           : bool
      ; crs_shown_in_todo_only_for_users_reviewing  : bool
      ; xcrs_shown_in_todo_only_for_users_reviewing : bool
      ; next_bookmark_update      : Next_bookmark_update.V1.t
      ; has_bookmark              : bool
      ; tip                       : Rev.V1.t
      ; tip_facts                 : Rev_facts.V1.t Or_pending.V1.t
      ; base_is_ancestor_of_tip   : Rev_facts.Is_ancestor.V1.t Or_pending.V1.t
      ; description               : string
      ; is_permanent              : bool
      ; seconder                  : User_name.V1.t option
      ; review_is_enabled         : bool
      ; reviewing                 : Reviewing.V1.t
      ; allow_review_for          : Allow_review_for.V1.t
      ; included_features         : Released_feature.V2.t list
      ; properties                : Properties.V1.t
      ; remote_repo_path          : Remote_repo_path.V1.t
      ; has_children              : bool
      ; release_process           : Release_process.V1.t
      ; who_can_release_into_me   : Who_can_release_into_me.V1.t
      ; send_email_to             : Email_address.V1.Set.t
      ; send_email_upon           : Send_email_upon.V1.Set.t
      ; locked                    : (Lock_name.V1.t * Locked.V1.t list) list
      }
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 39407c0d3e6261681a4bbd5036f004fa |}]
    ;;

    let of_model m =
      let { V13.
            feature_id
          ; feature_path
          ; rev_zero
          ; whole_feature_reviewers
          ; owners
          ; base
          ; base_facts
          ; crs_are_enabled
          ; crs_shown_in_todo_only_for_users_reviewing
          ; xcrs_shown_in_todo_only_for_users_reviewing
          ; next_bookmark_update
          ; has_bookmark
          ; tip
          ; tip_facts
          ; base_is_ancestor_of_tip
          ; description
          ; is_permanent
          ; seconder
          ; review_is_enabled
          ; reviewing
          ; allow_review_for
          ; included_features
          ; properties
          ; remote_repo_path
          ; has_children
          ; release_process
          ; who_can_release_into_me
          ; send_email_to
          ; send_email_upon
          ; locked
          ; _
          } = V13.of_model m in
      { feature_id
      ; feature_path
      ; rev_zero
      ; whole_feature_reviewers
      ; owners
      ; base
      ; base_facts
      ; crs_are_enabled
      ; crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; next_bookmark_update
      ; has_bookmark
      ; tip
      ; tip_facts
      ; base_is_ancestor_of_tip
      ; description
      ; is_permanent
      ; seconder
      ; review_is_enabled
      ; reviewing
      ; allow_review_for
      ; included_features
      ; properties
      ; remote_repo_path
      ; has_children
      ; release_process
      ; who_can_release_into_me
      ; send_email_to
      ; send_email_upon
      ; locked
      }
    ;;
  end

  module V11 = struct
    type t =
      { feature_id                : Feature_id.V1.t
      ; feature_path              : Feature_path.V1.t
      ; rev_zero                  : Rev.V1.t
      ; whole_feature_reviewers   : User_name.V1.Set.t
      ; owners                    : User_name.V1.t list
      ; base                      : Rev.V1.t
      ; base_facts                : Rev_facts.V1.t Or_pending.V1.t
      ; crs_are_enabled           : bool
      ; crs_shown_in_todo_only_for_users_reviewing  : bool
      ; xcrs_shown_in_todo_only_for_users_reviewing : bool
      ; next_bookmark_update      : Next_bookmark_update.V1.t
      ; has_bookmark              : bool
      ; tip                       : Rev.V1.t
      ; tip_facts                 : Rev_facts.V1.t Or_pending.V1.t
      ; base_is_ancestor_of_tip   : Rev_facts.Is_ancestor.V1.t Or_pending.V1.t
      ; description               : string
      ; is_permanent              : bool
      ; seconder                  : User_name.V1.t option
      ; review_is_enabled         : bool
      ; reviewing                 : Reviewing.V1.t
      ; allow_review_for          : Allow_review_for.V1.t
      ; included_features         : Released_feature.V2.t list
      ; properties                : Properties.V1.t
      ; remote_repo_path          : Remote_repo_path.V1.t
      ; has_children              : bool
      ; release_process           : Release_process.V1.t
      ; who_can_release_into_me   : Who_can_release_into_me.V1.t
      ; send_release_email_to     : Email_address.V1.Set.t
      ; should_send_release_email : bool
      ; locked                    : (Lock_name.V1.t * Locked.V1.t list) list
      }
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 262d0dfecb185a16db850185641581dd |}]
    ;;

    open! Core
    open! Import

    let of_model m =
      let { V12.
            feature_id
          ; feature_path
          ; rev_zero
          ; whole_feature_reviewers
          ; owners
          ; base
          ; base_facts
          ; crs_are_enabled
          ; crs_shown_in_todo_only_for_users_reviewing
          ; xcrs_shown_in_todo_only_for_users_reviewing
          ; next_bookmark_update
          ; has_bookmark
          ; tip
          ; tip_facts
          ; base_is_ancestor_of_tip
          ; description
          ; is_permanent
          ; seconder
          ; review_is_enabled
          ; reviewing
          ; allow_review_for
          ; included_features
          ; properties
          ; remote_repo_path
          ; has_children
          ; release_process
          ; who_can_release_into_me
          ; send_email_to
          ; send_email_upon
          ; locked
          ; _
          } = V12.of_model m in
      { feature_id
      ; feature_path
      ; rev_zero
      ; whole_feature_reviewers
      ; owners
      ; base
      ; base_facts
      ; crs_are_enabled
      ; crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; next_bookmark_update
      ; has_bookmark
      ; tip
      ; tip_facts
      ; base_is_ancestor_of_tip
      ; description
      ; is_permanent
      ; seconder
      ; review_is_enabled
      ; reviewing
      ; allow_review_for
      ; included_features
      ; properties
      ; remote_repo_path
      ; has_children
      ; release_process
      ; who_can_release_into_me
      ; send_release_email_to     = send_email_to
      ; should_send_release_email = Set.mem send_email_upon Release
      ; locked
      }
    ;;
  end

  module V10 = struct
    type t =
      { feature_id                : Feature_id.V1.t
      ; feature_path              : Feature_path.V1.t
      ; rev_zero                  : Rev.V1.t
      ; whole_feature_reviewers   : User_name.V1.Set.t
      ; owners                    : User_name.V1.t list
      ; base                      : Rev.V1.t
      ; base_facts                : Rev_facts.V1.t Or_pending.V1.t
      ; crs_are_enabled           : bool
      ; crs_shown_in_todo_only_for_users_reviewing  : bool
      ; xcrs_shown_in_todo_only_for_users_reviewing : bool
      ; next_bookmark_update      : Next_bookmark_update.V1.t
      ; has_bookmark              : bool
      ; tip                       : Rev.V1.t
      ; tip_facts                 : Rev_facts.V1.t Or_pending.V1.t
      ; base_is_ancestor_of_tip   : Rev_facts.Is_ancestor.V1.t Or_pending.V1.t
      ; description               : string
      ; is_permanent              : bool
      ; seconder                  : User_name.V1.t option
      ; review_is_enabled         : bool
      ; reviewing                 : Reviewing.V1.t
      ; included_features         : Released_feature.V2.t list
      ; properties                : Properties.V1.t
      ; remote_repo_path          : Remote_repo_path.V1.t
      ; has_children              : bool
      ; release_process           : Release_process.V1.t
      ; who_can_release_into_me   : Who_can_release_into_me.V1.t
      ; send_release_email_to     : Email_address.V1.Set.t
      ; should_send_release_email : bool
      ; locked                    : (Lock_name.V1.t * Locked.V1.t list) list
      }
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 27db5d5abdd26543c44bdcceacd46c2d |}]
    ;;

    let of_model m =
      let { V11.
            feature_id
          ; feature_path
          ; rev_zero
          ; whole_feature_reviewers
          ; owners
          ; base
          ; base_facts
          ; crs_are_enabled
          ; crs_shown_in_todo_only_for_users_reviewing
          ; xcrs_shown_in_todo_only_for_users_reviewing
          ; next_bookmark_update
          ; has_bookmark
          ; tip
          ; tip_facts
          ; base_is_ancestor_of_tip
          ; description
          ; is_permanent
          ; seconder
          ; review_is_enabled
          ; reviewing
          ; included_features
          ; properties
          ; remote_repo_path
          ; has_children
          ; release_process
          ; who_can_release_into_me
          ; send_release_email_to
          ; should_send_release_email
          ; locked
          ; _
          } = V11.of_model m in
      { feature_id
      ; feature_path
      ; rev_zero
      ; whole_feature_reviewers
      ; owners
      ; base
      ; base_facts
      ; crs_are_enabled
      ; crs_shown_in_todo_only_for_users_reviewing
      ; xcrs_shown_in_todo_only_for_users_reviewing
      ; next_bookmark_update
      ; has_bookmark
      ; tip
      ; tip_facts
      ; base_is_ancestor_of_tip
      ; description
      ; is_permanent
      ; seconder
      ; review_is_enabled
      ; reviewing
      ; included_features
      ; properties
      ; remote_repo_path
      ; has_children
      ; release_process
      ; who_can_release_into_me
      ; send_release_email_to
      ; should_send_release_email
      ; locked
      }
    ;;
  end

  module Model = V23
end

open! Core
open! Import

include Stable.Model

module Default_values = struct
  let crs_shown_in_todo_only_for_users_reviewing  = false
  let xcrs_shown_in_todo_only_for_users_reviewing = false
end

module Locked = struct
  (* We redefine [sexp_of_t] so that [at] is displayed more nicely, using [Time.sexp_of_t]
     rather than [Time.V1_round_trippable.sexp_of_t]. *)
  type t = Stable.Locked.Model.t =
    { by           : User_name.t
    ; reason       : string
    ; at           : Time.t
    ; is_permanent : bool
    }
  [@@deriving sexp_of]
end

module Sorted_by = struct
  type t = Stable.Sorted_by.Model.t [@@deriving compare, sexp_of]
end

let released_features t ~sorted_by =
  let module Released_feature = Iron_hg.Std.Released_feature in
  let by_release_time_decreasing =
    let rec aux r (released_feature : Released_feature.t) =
      List.fold ~init:(released_feature :: r) ~f:aux released_feature.includes
    in
    List.fold ~init:[] ~f:aux t.included_features
  in
  match (sorted_by : Sorted_by.t) with
  | `Release_time_decreasing -> by_release_time_decreasing
  | `Release_time -> List.rev by_release_time_decreasing
  | `Name ->
    List.sort by_release_time_decreasing ~cmp:(fun (r1 : Released_feature.t) r2 ->
      Feature_path.compare r1.feature_path r2.feature_path)
;;

let user_is_currently_reviewing t user =
  t.review_is_enabled
  && Reviewing.mem t.reviewing user
       ~whole_feature_reviewers:t.whole_feature_reviewers
       ~whole_feature_followers:t.whole_feature_followers
       ~is_seconded:(Option.is_some t.seconder)
;;

let reviewers_exn t ~sort =
  let sort =
    match sort with
    | `Alphabetically -> List.sort ~cmp:(fun (u1, _) (u2, _) -> User_name.compare u1 u2)
    | `Decreasing_review -> Line_count.sort_decreasing_review
  in
  ok_exn t.line_count_by_user
  |> sort
  |> List.map ~f:fst
;;

let who_can_review_exn t =
  ok_exn t.line_count_by_user
  |> List.filter_map ~f:(fun (username, (line_count : Line_count.t)) ->
    if user_is_currently_reviewing t username
    && (Review_or_commit.count (Line_count.to_review_column_shown line_count) > 0
        || line_count.review.follow > 0)
    then Some username
    else None)
  |> User_name.Set.of_list
;;

let reviewer_in_feature t user_name =
  { Reviewer.
    user_name
  ; is_whole_feature_follower = Set.mem t.whole_feature_followers user_name
  ; is_whole_feature_reviewer = Set.mem t.whole_feature_reviewers user_name
  }
;;

let recover_diff_of_its_latest_release t =
  match t.latest_release with
  | None -> None
  | Some { released_feature; diff_from_base_to_tip } ->
    let is_released_feature =
      Is_archived.to_bool t.is_archived
      && Rev.Compare_by_hash.equal t.tip t.base
      && Rev.Compare_by_hash.equal t.tip released_feature.tip
    in
    if not is_released_feature
    then None
    else (
      let { Released_feature.
            feature_id               = _
          ; feature_path             = _
          ; description              = _
          ; owners
          ; whole_feature_followers
          ; whole_feature_reviewers
          ; seconder
          ; base
          ; tip
          ; properties               = _
          ; includes
          ; release_cause            = _
          }
        = released_feature
      in
      Some { t with
             owners
           ; whole_feature_followers
           ; whole_feature_reviewers
           ; seconder                = Some seconder
           ; base
           ; tip
           ; included_features       = includes
           ; diff_from_base_to_tip   = Known (Ok diff_from_base_to_tip)
           ; latest_release          = None
           })
;;
