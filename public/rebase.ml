module Stable = struct

  open! Import_stable

  module Validation_command = struct
    module V1 = struct
      type t =
        { prog : string
        ; args : string list
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 34bb314d3ec6c154fc3190c05efb150c |}]
      ;;
    end

    module Model = V1
  end

  module Action = struct
    module V2 = struct
      type t =
        { feature_path                : Feature_path.V1.t
        ; allow_non_cr_clean_new_base : bool
        ; for_                        : User_name.V1.t
        ; new_base                    : Raw_rev.V1.t option
        ; abort_on_merge_conflicts    : bool
        ; post_merge_validation_hook  : Validation_command.V1.t option
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 9becbb69b219e80dda7bd7586c18f1c2 |}]
      ;;

      let to_model m = m
    end

    module V1 = struct
      type t =
        { feature_path                : Feature_path.V1.t
        ; allow_non_cr_clean_new_base : bool
        ; for_                        : User_name.V1.t
        ; new_base                    : Raw_rev.V1.t option
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 66e1c2b0ce3291c4c5b1b8e5fae4b0dc |}]
      ;;

      let to_model { feature_path
                   ; allow_non_cr_clean_new_base
                   ; for_
                   ; new_base
                   } =
        V2.to_model
          { V2.
            feature_path
          ; allow_non_cr_clean_new_base
          ; for_
          ; new_base
          ; abort_on_merge_conflicts = false
          ; post_merge_validation_hook = None
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
    (struct let name = "rebase" end)
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action             = Stable.Action.             Model
module Reaction           = Stable.Reaction.           Model
module Validation_command = Stable.Validation_command. Model
