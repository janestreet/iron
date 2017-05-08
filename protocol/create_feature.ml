module Stable = struct

  open! Import_stable

  module Action = struct

    module V5 = struct
      type t =
        { feature_path                : Feature_path.V1.t
        ; owners                      : User_name.V1.t list
        ; is_permanent                : bool
        ; description                 : string
        ; base                        : Rev.V1.t option
        ; tip                         : Rev.V1.t option
        ; add_whole_feature_reviewers : User_name.V1.Set.t
        ; reviewing                   : [ `Whole_feature_reviewers
                                        | `First_owner
                                        ]
        ; rev_zero                    : Rev.V1.t
        ; remote_repo_path            : Remote_repo_path.V1.t option
        ; allow_non_cr_clean_base     : bool
        ; properties                  : Properties.V1.t option
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| e59b69ea75b5f84b413b8701711eedaa |}]
      ;;

      let to_model (t : t) = t
    end

    module V4 = struct
      type t =
        { feature_path                : Feature_path.V1.t
        ; owners                      : User_name.V1.t list
        ; is_permanent                : bool
        ; description                 : string
        ; base                        : Rev.V1.t option
        ; tip                         : Rev.V1.t option
        ; add_whole_feature_reviewers : User_name.V1.Set.t
        ; rev_zero                    : Rev.V1.t
        ; remote_repo_path            : Remote_repo_path.V1.t option
        ; allow_non_cr_clean_base     : bool
        ; properties                  : Properties.V1.t option
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 018562a8b5512ee89ab91a7d99ca70cf |}]
      ;;

      let to_model
            { feature_path
            ; owners
            ; is_permanent
            ; description
            ; base
            ; tip
            ; add_whole_feature_reviewers
            ; rev_zero
            ; remote_repo_path
            ; allow_non_cr_clean_base
            ; properties
            } =
        V5.to_model
          { feature_path
          ; owners
          ; is_permanent
          ; description
          ; base
          ; tip
          ; add_whole_feature_reviewers
          ; reviewing = `Whole_feature_reviewers
          ; rev_zero
          ; remote_repo_path
          ; allow_non_cr_clean_base
          ; properties
          }
      ;;
    end

    module Model = V5
  end

  module Reaction = struct
    module V4 = struct
      type t =
        { feature_id       : Feature_id.V1.t
        ; remote_repo_path : Remote_repo_path.V1.t
        ; tip              : Rev.V1.t
        }
      [@@deriving bin_io, fields, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b8fb00961ce76152e48184be82e52d5a |}]
      ;;

      let of_model (t : t) = t
    end

    module V3 = struct
      type t =
        { remote_repo_path : Remote_repo_path.V1.t
        ; tip              : Rev.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 459d2910cafe370222840499543ac843 |}]
      ;;

      let of_model m =
        let { V4.
              feature_id = _
            ; remote_repo_path
            ; tip
            } = V4.of_model m in
        { remote_repo_path
        ; tip
        }
      ;;
    end

    module Model = V4
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "create-feature" end)
    (struct let version = 7 end)
    (Stable.Action.V5)
    (Stable.Reaction.V4)

include Register_old_rpc
    (struct let version = 6 end)
    (Stable.Action.V4)
    (Stable.Reaction.V4)

include Register_old_rpc
    (struct let version = 5 end)
    (Stable.Action.V4)
    (Stable.Reaction.V3)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
