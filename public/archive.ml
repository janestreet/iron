module Stable = struct

  open! Import_stable

  module Action = struct
    module V2 = struct
      type t =
        { feature_path         : Feature_path.V1.t
        ; for_                 : User_name.V1.t
        ; reason_for_archiving : string
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| d062de2d22c8e473e10b885cef1e989b |}]
      ;;

      let to_model (m : t) = m
    end

    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; for_         : User_name.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| caa8c6bc0b63d59c3c306d8e2483b33e |}]
      ;;

      let to_model { feature_path; for_ } =
        V2.to_model { feature_path
                    ; for_
                    ; reason_for_archiving = ""
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
    (struct let name = "archive" end)
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
