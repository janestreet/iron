module Stable = struct

  open! Import_stable

  (* [for_] is for testing mainly. Catch_up on behalf of someone else will be rejected
     later on during the [catch_up_diffs] query *)
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

  module Catch_up_session = struct
    module V6 = struct
      type t =
        { catch_up_session_id              : Session_id.V1.t
        ; catch_up_session_tip             : Rev.V1.t
        ; creation_time                    : Time.V1_round_trippable.t
        ; reviewer_in_session              : Reviewer.V2.t
        ; diff4s_to_catch_up               : Diff4_to_catch_up.V3.t list
        ; line_count_remaining_to_catch_up : Line_count.Catch_up.V1.t
        ; remote_rev_zero                  : Rev.V1.t
        ; remote_repo_path                 : Remote_repo_path.V1.t
        ; feature_path                     : Feature_path.V1.t
        ; feature_id                       : Feature_id.V1.t
        ; whole_feature_reviewers          : User_name.V1.Set.t
        ; owners                           : User_name.V1.t list
        ; base                             : Rev.V1.t
        ; tip                              : Rev.V1.t
        ; description                      : string
        ; is_permanent                     : bool
        ; is_archived                      : Is_archived.V1.t
        ; seconder                         : User_name.V1.t option
        ; lines_required_to_separate_ddiff_hunks : int
        }
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 149b1c9200a3a0e86f98ab59d4f9e48f |}]
      ;;
    end

    module V5 = struct
      type t =
        { catch_up_session_id              : Session_id.V1.t
        ; catch_up_session_tip             : Rev.V1.t
        ; creation_time                    : Time.V1_round_trippable.t
        ; reviewer_in_session              : Reviewer.V2.t
        ; diff4s_to_catch_up               : Diff4_to_catch_up.V3.t list
        ; line_count_remaining_to_catch_up : Line_count.Catch_up.V1.t
        ; remote_rev_zero                  : Rev.V1.t
        ; remote_repo_path                 : Remote_repo_path.V1.t
        ; feature_path                     : Feature_path.V1.t
        ; feature_id                       : Feature_id.V1.t
        ; whole_feature_reviewers          : User_name.V1.Set.t
        ; owners                           : User_name.V1.t list
        ; base                             : Rev.V1.t
        ; tip                              : Rev.V1.t
        ; description                      : string
        ; is_permanent                     : bool
        ; is_archived                      : bool
        ; seconder                         : User_name.V1.t option
        ; lines_required_to_separate_ddiff_hunks : int
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 28258a680554a97f026a9b004b26e495 |}]
      ;;

      open! Core
      open! Import

      let of_v6
            { V6.
              catch_up_session_id
            ; catch_up_session_tip
            ; creation_time
            ; reviewer_in_session
            ; diff4s_to_catch_up
            ; line_count_remaining_to_catch_up
            ; remote_rev_zero
            ; remote_repo_path
            ; feature_path
            ; feature_id
            ; whole_feature_reviewers
            ; owners
            ; base
            ; tip
            ; description
            ; is_permanent
            ; is_archived
            ; seconder
            ; lines_required_to_separate_ddiff_hunks
            } =
        { catch_up_session_id
        ; catch_up_session_tip
        ; creation_time
        ; reviewer_in_session
        ; diff4s_to_catch_up
        ; line_count_remaining_to_catch_up
        ; remote_rev_zero
        ; remote_repo_path
        ; feature_path
        ; feature_id
        ; whole_feature_reviewers
        ; owners
        ; base
        ; tip
        ; description
        ; is_permanent
        ; is_archived = Is_archived.Stable.V1.to_bool is_archived
        ; seconder
        ; lines_required_to_separate_ddiff_hunks
        }
      ;;
    end

    module V4 = struct
      type t =
        { catch_up_session_id              : Session_id.V1.t
        ; catch_up_session_tip             : Rev.V1.t
        ; creation_time                    : Time.V1_round_trippable.t
        ; reviewer_in_session              : Reviewer.V2.t
        ; diff4s_to_catch_up               : Diff4_to_catch_up.V3.t list
        ; line_count_remaining_to_catch_up : Line_count.Catch_up.V1.t
        ; remote_rev_zero                  : Rev.V1.t
        ; remote_repo_path                 : Remote_repo_path.V1.t
        ; feature_path                     : Feature_path.V1.t
        ; feature_id                       : Feature_id.V1.t
        ; whole_feature_reviewers          : User_name.V1.Set.t
        ; owners                           : User_name.V1.t list
        ; base                             : Rev.V1.t
        ; tip                              : Rev.V1.t
        ; description                      : string
        ; is_permanent                     : bool
        ; is_archived                      : bool
        ; seconder                         : User_name.V1.t option
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| c0965e955ce0be7f9a5659dcc60c9e82 |}]
      ;;

      open! Core
      open! Import

      let of_v5
            { V5.
              catch_up_session_id
            ; catch_up_session_tip
            ; creation_time
            ; reviewer_in_session
            ; diff4s_to_catch_up
            ; line_count_remaining_to_catch_up
            ; remote_rev_zero
            ; remote_repo_path
            ; feature_path
            ; feature_id
            ; whole_feature_reviewers
            ; owners
            ; base
            ; tip
            ; description
            ; is_permanent
            ; is_archived
            ; seconder
            ; _
            } =
        { catch_up_session_id
        ; catch_up_session_tip
        ; creation_time
        ; reviewer_in_session
        ; diff4s_to_catch_up
        ; line_count_remaining_to_catch_up
        ; remote_rev_zero
        ; remote_repo_path
        ; feature_path
        ; feature_id
        ; whole_feature_reviewers
        ; owners
        ; base
        ; tip
        ; description
        ; is_permanent
        ; is_archived
        ; seconder
        }
      ;;
    end

    module V3 = struct
      type t =
        { catch_up_session_id             : Session_id.V1.t
        ; catch_up_session_tip            : Rev.V1.t
        ; creation_time                   : Time.V1_round_trippable.t
        ; reviewer_in_session             : Reviewer.V2.t
        ; diff4s_to_catch_up              : Diff4_to_catch_up.V3.t list
        ; num_lines_remaining_to_catch_up : int
        ; remote_rev_zero                 : Rev.V1.t
        ; remote_repo_path                : Remote_repo_path.V1.t
        ; feature_path                    : Feature_path.V1.t
        ; feature_id                      : Feature_id.V1.t
        ; whole_feature_reviewers         : User_name.V1.Set.t
        ; owners                          : User_name.V1.t list
        ; base                            : Rev.V1.t
        ; tip                             : Rev.V1.t
        ; description                     : string
        ; is_permanent                    : bool
        ; is_archived                     : bool
        ; seconder                        : User_name.V1.t option
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| dc15c5fcd2c50b030a0fe01be0cbd72e |}]
      ;;

      open! Core
      open! Import

      let of_v4
            { V4.
              catch_up_session_id
            ; catch_up_session_tip
            ; creation_time
            ; reviewer_in_session
            ; diff4s_to_catch_up
            ; line_count_remaining_to_catch_up
            ; remote_rev_zero
            ; remote_repo_path
            ; feature_path
            ; feature_id
            ; whole_feature_reviewers
            ; owners
            ; base
            ; tip
            ; description
            ; is_permanent
            ; is_archived
            ; seconder
            } =
        { catch_up_session_id
        ; catch_up_session_tip
        ; creation_time
        ; reviewer_in_session
        ; diff4s_to_catch_up
        ; num_lines_remaining_to_catch_up
          = Line_count.Catch_up.total line_count_remaining_to_catch_up
        ; remote_rev_zero
        ; remote_repo_path
        ; feature_path
        ; feature_id
        ; whole_feature_reviewers
        ; owners
        ; base
        ; tip
        ; description
        ; is_permanent
        ; is_archived
        ; seconder
        }
      ;;
    end

    module V2 = struct
      type t =
        { catch_up_session_id             : Session_id.V1.t
        ; catch_up_session_tip            : Rev.V1.t
        ; creation_time                   : Time.V1_round_trippable.t
        ; diff4s_to_catch_up              : Diff4_to_catch_up.V2.t list
        ; num_lines_remaining_to_catch_up : int
        ; remote_rev_zero                 : Rev.V1.t
        ; remote_repo_path                : Remote_repo_path.V1.t
        ; feature_path                    : Feature_path.V1.t
        ; feature_id                      : Feature_id.V1.t
        ; whole_feature_reviewers         : User_name.V1.Set.t
        ; owners                          : User_name.V1.t list
        ; base                            : Rev.V1.t
        ; tip                             : Rev.V1.t
        ; description                     : string
        ; is_permanent                    : bool
        ; is_archived                     : bool
        ; seconder                        : User_name.V1.t option
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| a2abaf42825f733295086add2b8da421 |}]
      ;;

      open! Core
      open! Import

      let of_v3
            { V3.
              catch_up_session_id
            ; catch_up_session_tip
            ; creation_time
            ; diff4s_to_catch_up
            ; num_lines_remaining_to_catch_up
            ; remote_rev_zero
            ; remote_repo_path
            ; feature_path
            ; feature_id
            ; whole_feature_reviewers
            ; owners
            ; base
            ; tip
            ; description
            ; is_permanent
            ; is_archived
            ; seconder
            ; _
            } =
        { catch_up_session_id
        ; catch_up_session_tip
        ; creation_time
        ; diff4s_to_catch_up
          = List.map diff4s_to_catch_up ~f:Diff4_to_catch_up.Stable.V2.of_v3
        ; num_lines_remaining_to_catch_up
        ; remote_rev_zero
        ; remote_repo_path
        ; feature_path
        ; feature_id
        ; whole_feature_reviewers
        ; owners
        ; base
        ; tip
        ; description
        ; is_permanent
        ; is_archived
        ; seconder
        }
      ;;
    end

    module Model = V6
  end

  module Reaction = struct
    module V6 = struct
      type t =
        [ `Up_to_date
        | `Catch_up_session of Catch_up_session.V6.t
        ]
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 444c84e444f72a4e50a94b67c82a24a7 |}]
      ;;

      let of_model m = m
    end

    module V5 = struct
      type t =
        [ `Up_to_date
        | `Catch_up_session of Catch_up_session.V5.t
        ]
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| cca2f20b1058574c30d37eebba9c5318 |}]
      ;;

      let of_model m =
        match V6.of_model m with
        | `Up_to_date as t -> t
        | `Catch_up_session v6 -> `Catch_up_session (Catch_up_session.V5.of_v6 v6)
      ;;
    end

    module V4 = struct
      type t =
        [ `Up_to_date
        | `Catch_up_session of Catch_up_session.V4.t
        ]
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8071ba9f703dbfd57c79677c8ab92dfb |}]
      ;;

      let of_model m =
        match V5.of_model m with
        | `Up_to_date as t -> t
        | `Catch_up_session v5 -> `Catch_up_session (Catch_up_session.V4.of_v5 v5)
      ;;
    end

    module V3 = struct
      type t =
        { status : [ `Up_to_date
                   | `Catch_up_session of Catch_up_session.V3.t
                   ]
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 215725f25e966e5593e9926461b8638b |}]
      ;;

      let of_model m =
        { status
          = match V4.of_model m with
            | `Up_to_date as t -> t
            | `Catch_up_session v4 -> `Catch_up_session (Catch_up_session.V3.of_v4 v4)
        }
      ;;
    end

    module V2 = struct
      type t =
        { status : [ `Up_to_date
                   | `Catch_up_session of Catch_up_session.V2.t
                   ]
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| e2fd9d68e43ead6f37dea3189b7473de |}]
      ;;

      let of_model m =
        let { V3. status } = V3.of_model m in
        { status
          = match status with
            | `Up_to_date as t -> t
            | `Catch_up_session v3 -> `Catch_up_session (Catch_up_session.V2.of_v3 v3)
        }
      ;;
    end

    module Model = V6
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "get-catch-up-session" end)
    (struct let version = 6 end)
    (Stable.Action.V1)
    (Stable.Reaction.V6)

include Register_old_rpc
    (struct let version = 5 end)
    (Stable.Action.V1)
    (Stable.Reaction.V5)

include Register_old_rpc
    (struct let version = 4 end)
    (Stable.Action.V1)
    (Stable.Reaction.V4)

include Register_old_rpc
    (struct let version = 3 end)
    (Stable.Action.V1)
    (Stable.Reaction.V3)

include Register_old_rpc
    (struct let version = 2 end)
    (Stable.Action.V1)
    (Stable.Reaction.V2)

module Action           = Stable.Action.           Model
module Reaction         = Stable.Reaction.         Model
module Catch_up_session = Stable.Catch_up_session. Model
