module Stable = struct

  open! Import_stable

  module Action = struct
    module V3 = struct
      type t =
        { feature_path   : Feature_path.V1.t
        ; for_           : User_name.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| caa8c6bc0b63d59c3c306d8e2483b33e |}]
      ;;

      let to_model t = t
    end

    module V2 = struct
      type t =
        { feature_path   : Feature_path.V1.t
        ; for_           : User_name.V1.t
        ; even_if_locked : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 99a14253de3cc4fcea99f8e47047bf85 |}]
      ;;

      let to_model { feature_path
                   ; for_
                   ; even_if_locked = _
                   } =
        V3.to_model
          { V3.
            feature_path
          ; for_
          }
      ;;
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
        V3.to_model
          { V3.
            feature_path
          ; for_
          }
      ;;
    end

    module Model = V3
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "compress" end)
    (struct let version = 3 end)
    (Stable.Action.V3)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
