module Stable = struct

  open! Import_stable

  module Action = struct
    module V4 = struct
      type t =
        { feature_path     : Feature_path.V1.t
        ; rev_zero         : Rev.V1.t option
        ; for_             : User_name.V1.t
        ; ensure_reviewing : bool
        ; which_session    : Which_session.V1.t
        ; lock_session     : [ `If_applicable | `No ]
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 76694ee9077beeb5832c9deb6e663de9 |}]
      ;;

      let to_model (t : t) = t
    end

    module V3 = struct
      type t =
        { feature_path     : Feature_path.V1.t
        ; rev_zero         : Rev.V1.t option
        ; for_             : User_name.V1.t
        ; ensure_reviewing : bool
        ; which_session    : Which_session.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ac28676b009899a5e4a3bcb68c79f617 |}]
      ;;

      let to_model { feature_path; rev_zero; for_; ensure_reviewing; which_session } =
        V4.to_model { feature_path
                    ; rev_zero
                    ; for_
                    ; ensure_reviewing
                    ; which_session
                    ; lock_session = `No
                    }
      ;;
    end

    module V2 = struct
      type t =
        { feature_path     : Feature_path.V1.t
        ; rev_zero         : Rev.V1.t option
        ; for_             : User_name.V1.t
        ; ensure_reviewing : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f4f0758c944b8718427fbda4f1a0e85d |}]
      ;;

      let to_model { feature_path; rev_zero; for_; ensure_reviewing } =
        V3.to_model { feature_path
                    ; rev_zero
                    ; for_
                    ; ensure_reviewing
                    ; which_session    = Current_session
                    }
      ;;
    end

    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; rev_zero     : Rev.V1.t option
        ; for_         : User_name.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 95654cca7533089b6d772a1108aa3c17 |}]
      ;;

      let to_model { feature_path; rev_zero; for_ } =
        V2.to_model { feature_path; rev_zero; for_; ensure_reviewing = true }
      ;;
    end

    module Model = V4
  end

  module Line_count_to_goal = struct
    module V1 = struct
      type 'a t =
        { from_session_end                    : 'a
        ; from_brain_if_session_was_committed : 'a
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: Bin_digest_type_variable.tick_a t];
        [%expect {| e90a33d153d1b3b41d86c069e9e2b1c9 |}]
      ;;
    end

    module Model = V1
  end

  module Review_session = struct
    module V9 = struct
      type t =
        { review_session_id              : Session_id.V1.t
        ; review_session_tip             : Rev.V1.t
        ; reviewer_in_session            : Reviewer.V2.t
        ; reviewer_in_feature            : Reviewer.V2.t
        ; diff4s_in_session
          : Review_session.Diff4_in_session.And_review_kind.V2.t array
        ; may_be_reviewed_by             : Allow_review_for.Users.V1.t
        ; line_count_to_finish_session   : Line_count.Review.V1.t
        ; line_count_to_goal
          : Line_count.Review.V1.t Or_error.V2.t Or_pending.V1.t
              Line_count_to_goal.V1.t
        ; is_locked : bool
        ; lines_required_to_separate_ddiff_hunks : int
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ddd8f00f9535f58a566122ceb5adedd6 |}]
      ;;
    end

    module V8 = struct
      type t =
        { review_session_id              : Session_id.V1.t
        ; review_session_tip             : Rev.V1.t
        ; reviewer_in_session            : Reviewer.V2.t
        ; reviewer_in_feature            : Reviewer.V2.t
        ; diff4s_in_session
          : Review_session.Diff4_in_session.And_review_kind.V2.t array
        ; may_be_reviewed_by             : Allow_review_for.Users.V1.t
        ; line_count_to_finish_session   : Line_count.Review.V1.t
        ; line_count_to_goal
          : Line_count.Review.V1.t Or_error.V2.t Or_pending.V1.t
              Line_count_to_goal.V1.t
        ; is_locked : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8022f82fa771ad92056ca2420f6c20f1 |}]
      ;;

      open! Core
      open! Import

      let of_v9 { V9.
                  review_session_id
                ; review_session_tip
                ; reviewer_in_session
                ; reviewer_in_feature
                ; diff4s_in_session
                ; may_be_reviewed_by
                ; line_count_to_finish_session
                ; line_count_to_goal
                ; is_locked
                ; _
                } =
        { review_session_id
        ; review_session_tip
        ; reviewer_in_session
        ; reviewer_in_feature
        ; diff4s_in_session
        ; may_be_reviewed_by
        ; line_count_to_finish_session
        ; line_count_to_goal
        ; is_locked
        }
      ;;
    end

    module V7 = struct
      type t =
        { review_session_id              : Session_id.V1.t
        ; review_session_tip             : Rev.V1.t
        ; reviewer_in_session            : Reviewer.V2.t
        ; reviewer_in_feature            : Reviewer.V2.t
        ; diff4s_in_session
          : Review_session.Diff4_in_session.And_review_kind.V2.t array
        ; may_be_reviewed_by             : Allow_review_for.Users.V1.t
        ; num_lines_remaining_to_review_in_session : int
        ; line_count_to_goal
          : Line_count.Review.V1.t Or_error.V2.t Or_pending.V1.t
              Line_count_to_goal.V1.t
        ; user_is_using_locked_sessions : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 9e24061dbccd6821372183e4740ff15f |}]
      ;;

      open! Core
      open! Import

      let of_v8 { V8.
                  review_session_id
                ; review_session_tip
                ; reviewer_in_session
                ; reviewer_in_feature
                ; diff4s_in_session
                ; may_be_reviewed_by
                ; line_count_to_finish_session
                ; line_count_to_goal
                ; is_locked = _
                } =
        let num_lines_remaining_to_review_in_session =
          Line_count.Review.total line_count_to_finish_session
        in
        { review_session_id
        ; review_session_tip
        ; reviewer_in_session
        ; reviewer_in_feature
        ; diff4s_in_session
        ; may_be_reviewed_by
        ; num_lines_remaining_to_review_in_session
        ; line_count_to_goal
        ; user_is_using_locked_sessions = true
        }
      ;;
    end

    module V6 = struct
      type t =
        { review_session_id             : Session_id.V1.t
        ; review_session_tip            : Rev.V1.t
        ; reviewer_in_session           : Reviewer.V2.t
        ; reviewer_in_feature           : Reviewer.V2.t
        ; diff4s_in_session
          : Review_session.Diff4_in_session.And_review_kind.V2.t array
        ; num_lines_remaining_to_review : int
        ; may_be_reviewed_by            : Allow_review_for.Users.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 60299e8beab60fd3f22ab6ee4b1fc594 |}]
      ;;

      open! Core
      open! Import

      let of_v7 { V7.
                  review_session_id
                ; review_session_tip
                ; reviewer_in_session
                ; reviewer_in_feature
                ; diff4s_in_session
                ; may_be_reviewed_by
                ; num_lines_remaining_to_review_in_session
                ; line_count_to_goal = _
                ; user_is_using_locked_sessions = _
                } =
        { review_session_id
        ; review_session_tip
        ; reviewer_in_session
        ; reviewer_in_feature
        ; diff4s_in_session
        ; num_lines_remaining_to_review = num_lines_remaining_to_review_in_session
        ; may_be_reviewed_by
        }
      ;;
    end

    module V5 = struct
      type t =
        { review_session_id             : Session_id.V1.t
        ; review_session_tip            : Rev.V1.t
        ; is_whole_feature_reviewer     : bool
        ; diff4s_in_session             : Review_session.Diff4_in_session.V2.t array
        ; num_lines_remaining_to_review : int
        ; may_be_reviewed_by            : Allow_review_for.Users.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 5f10ab212b2c048c697290d0e15b5bef |}]
      ;;

      open! Core
      open! Import

      let of_v6 { V6.
                  review_session_id
                ; review_session_tip
                ; reviewer_in_session
                ; diff4s_in_session
                ; num_lines_remaining_to_review
                ; may_be_reviewed_by
                ; _
                } =
        let diff4s_in_session =
          Array.map diff4s_in_session ~f:Diff4_in_session.And_review_kind.diff4_in_session
        in
        { review_session_id
        ; review_session_tip
        ; is_whole_feature_reviewer = reviewer_in_session.is_whole_feature_reviewer
        ; diff4s_in_session
        ; num_lines_remaining_to_review
        ; may_be_reviewed_by
        }
      ;;
    end

    module V4 = struct
      type t =
        { review_session_id             : Session_id.V1.t
        ; review_session_tip            : Rev.V1.t
        ; is_whole_feature_reviewer     : bool
        ; diff4s_in_session             : Review_session.Diff4_in_session.V2.t array
        ; num_lines_remaining_to_review : int
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b810c472c609f0df6f32a0eea3921f79 |}]
      ;;

      let of_v5 { V5.
                  review_session_id
                ; review_session_tip
                ; is_whole_feature_reviewer
                ; diff4s_in_session
                ; num_lines_remaining_to_review
                ; may_be_reviewed_by            = _
                } =
        { review_session_id
        ; review_session_tip
        ; is_whole_feature_reviewer
        ; diff4s_in_session
        ; num_lines_remaining_to_review
        }
      ;;
    end

    module Model = V9
  end

  module Status = struct
    module V1 = struct
      type 'a t =
        [ `Up_to_date
        | `Bookmark_update_is_pending
        | `Review_session of 'a
        ]
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: Bin_digest_type_variable.tick_a t];
        [%expect {| ab08c8ed9b62771b0a3c129ec29bfdc8 |}]
      ;;

      let map t ~f =
        match t with
        | `Up_to_date
        | `Bookmark_update_is_pending as t -> t
        | `Review_session a -> `Review_session (f a)
      ;;
    end
  end

  module Reaction = struct
    module V9 = struct
      type t =
        { status            : Review_session.V9.t Status.V1.t
        ; feature_tip       : Rev.V1.t
        ; remote_rev_zero   : Rev.V1.t
        ; remote_repo_path  : Remote_repo_path.V1.t
        ; may_second        : bool
        }
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 7b33649e1aacaa7b24c634cc0ef724e8 |}]
      ;;

      let of_model m = m
    end

    module V8 = struct
      type t =
        { status            : Review_session.V8.t Status.V1.t
        ; feature_tip       : Rev.V1.t
        ; remote_rev_zero   : Rev.V1.t
        ; remote_repo_path  : Remote_repo_path.V1.t
        ; may_second        : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 27240a9af46e697d08394d10d5695243 |}]
      ;;

      let of_model model =
        let { V9. status; feature_tip; remote_rev_zero; remote_repo_path; may_second }
          = V9.of_model model
        in
        { status           = Status.V1.map status ~f:Review_session.V8.of_v9
        ; feature_tip
        ; remote_rev_zero
        ; remote_repo_path
        ; may_second
        }
      ;;
    end

    module V7 = struct
      type t =
        { status            : Review_session.V7.t Status.V1.t
        ; feature_tip       : Rev.V1.t
        ; remote_rev_zero   : Rev.V1.t
        ; remote_repo_path  : Remote_repo_path.V1.t
        ; may_second        : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 0283f2194cb7b2691ca8cdfbd8a9c4f4 |}]
      ;;

      let of_model model =
        let { V8. status; feature_tip; remote_rev_zero; remote_repo_path; may_second }
          = V8.of_model model
        in
        { status           = Status.V1.map status ~f:Review_session.V7.of_v8
        ; feature_tip
        ; remote_rev_zero
        ; remote_repo_path
        ; may_second
        }
      ;;
    end

    module V6 = struct
      type t =
        { status            : Review_session.V6.t Status.V1.t
        ; feature_tip       : Rev.V1.t
        ; remote_rev_zero   : Rev.V1.t
        ; remote_repo_path  : Remote_repo_path.V1.t
        ; may_second        : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| bd439e1aaa5a52c80c31a63428cc2059 |}]
      ;;

      let of_model model =
        let { V7. status; feature_tip; remote_rev_zero; remote_repo_path; may_second }
          = V7.of_model model
        in
        { status           = Status.V1.map status ~f:Review_session.V6.of_v7
        ; feature_tip
        ; remote_rev_zero
        ; remote_repo_path
        ; may_second
        }
      ;;
    end

    module V5 = struct
      type t =
        { status            : Review_session.V5.t Status.V1.t
        ; feature_tip       : Rev.V1.t
        ; remote_rev_zero   : Rev.V1.t
        ; remote_repo_path  : Remote_repo_path.V1.t
        ; may_second        : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 7f1b01463998861a45970250da23f215 |}]
      ;;

      let of_model model =
        let { V6. status; feature_tip; remote_rev_zero; remote_repo_path; may_second }
          = V6.of_model model
        in
        { status           = Status.V1.map status ~f:Review_session.V5.of_v6
        ; feature_tip
        ; remote_rev_zero
        ; remote_repo_path
        ; may_second
        }
      ;;
    end

    module V4 = struct
      type t =
        { status            : Review_session.V4.t Status.V1.t
        ; feature_tip       : Rev.V1.t
        ; remote_rev_zero   : Rev.V1.t
        ; remote_repo_path  : Remote_repo_path.V1.t
        ; may_second        : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 773ca234c455a5a5a82165c27f89a9b5 |}]
      ;;

      let of_model model =
        let { V5. status; feature_tip; remote_rev_zero; remote_repo_path; may_second }
          = V5.of_model model
        in
        { status           = Status.V1.map status ~f:Review_session.V4.of_v5
        ; feature_tip
        ; remote_rev_zero
        ; remote_repo_path
        ; may_second
        }
      ;;
    end

    module Model = V9
  end
end

open! Core
open! Import

include Iron_versioned_rpc.Make
    (struct let name = "get-review-session" end)
    (struct let version = 11 end)
    (Stable.Action.V4)
    (Stable.Reaction.V9)

include Register_old_rpc
    (struct let version = 10 end)
    (Stable.Action.V4)
    (Stable.Reaction.V8)

include Register_old_rpc
    (struct let version = 9 end)
    (Stable.Action.V3)
    (Stable.Reaction.V7)

include Register_old_rpc
    (struct let version = 8 end)
    (Stable.Action.V3)
    (Stable.Reaction.V6)

include Register_old_rpc
    (struct let version = 7 end)
    (Stable.Action.V2)
    (Stable.Reaction.V6)

include Register_old_rpc
    (struct let version = 6 end)
    (Stable.Action.V2)
    (Stable.Reaction.V5)

include Register_old_rpc
    (struct let version = 5 end)
    (Stable.Action.V2)
    (Stable.Reaction.V4)

include Register_old_rpc
    (struct let version = 4 end)
    (Stable.Action.V1)
    (Stable.Reaction.V4)

include Register_map_reaction_in_client
    (struct
      (* The sort is done client side to avoid unnecessary work in Iron server. *)
      let of_server_reaction (action : action) (reaction : reaction) =
        (match reaction.status with
         | `Up_to_date | `Bookmark_update_is_pending -> ()
         | `Review_session review_session ->
           Array.sort review_session.diff4s_in_session ~cmp:(fun t1 t2 ->
             Diff4_in_session.compare_by_path_in_repo_at_f2_for_review
               t1.diff4_in_session
               t2.diff4_in_session));
        (match action.which_session with
         | Current_session -> ()
         | This_session supplied ->
           match reaction.status with
           | `Up_to_date | `Bookmark_update_is_pending ->
             Error.raise (Session_id.no_session_error ~supplied)
           | `Review_session review_session ->
             ok_exn (Session_id.check ~actual:review_session.review_session_id ~supplied));
        reaction
    end)

module Action         = Stable.Action.         Model
module Reaction       = Stable.Reaction.       Model
module Review_session = Stable.Review_session. Model

module Line_count_to_goal = struct
  include Stable.Line_count_to_goal. Model

  let map t ~f =
    { from_session_end                    = f t.from_session_end
    ; from_brain_if_session_was_committed = f t.from_brain_if_session_was_committed
    }
  ;;
end
