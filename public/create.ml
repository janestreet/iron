module Stable = struct

  open! Import_stable

  module Action = struct
    module V2 = struct

      type t =
        { feature_path                : Feature_path.V1.t
        ; base                        : Raw_rev.V1.t option
        ; tip                         : Raw_rev.V1.t option
        ; description                 : string option
        ; owners                      : User_name.V1.t list
        ; is_permanent                : bool
        ; remote_repo_path            : Remote_repo_path.V1.t option
        ; no_bookmark                 : bool
        ; add_whole_feature_reviewers : User_name.V1.Set.t option
        ; reviewing                   : [ `Whole_feature_reviewers
                                        | `First_owner
                                        ]
        ; allow_non_cr_clean_base     : bool
        ; properties                  : Properties.V1.t option
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b9bfba6d0e4296c129084d6140aedb16 |}]
      ;;

      let to_model (m : t) = m
    end

    module V1 = struct

      type t =
        { feature_path                : Feature_path.V1.t
        ; base                        : Raw_rev.V1.t option
        ; tip                         : Raw_rev.V1.t option
        ; description                 : string option
        ; owners                      : User_name.V1.t list
        ; is_permanent                : bool
        ; remote_repo_path            : Remote_repo_path.V1.t option
        ; no_bookmark                 : bool
        ; add_whole_feature_reviewers : User_name.V1.Set.t option
        ; allow_non_cr_clean_base     : bool
        ; properties                  : Properties.V1.t option
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ea6032921af2f1817d8b7b14ebc5f6d7 |}]
      ;;

      let to_model
            { feature_path
            ; base
            ; tip
            ; description
            ; owners
            ; is_permanent
            ; remote_repo_path
            ; no_bookmark
            ; add_whole_feature_reviewers
            ; allow_non_cr_clean_base
            ; properties
            } =
        V2.to_model
          { feature_path
          ; base
          ; tip
          ; description
          ; owners
          ; is_permanent
          ; remote_repo_path
          ; no_bookmark
          ; add_whole_feature_reviewers
          ; reviewing = `Whole_feature_reviewers
          ; allow_non_cr_clean_base
          ; properties
          }
      ;;
    end

    module Model = V2
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

include Iron_command_rpc.Make
    (struct let name = "create" end)
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
