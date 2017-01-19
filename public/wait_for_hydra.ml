module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path      : Feature_path.V1.t
        ; rev_zero          : Rev.V1.t option
        ; timeout           : Span.V1.t
        ; whether_to_update : [ `No_update
                              | `Update
                              ]
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 34e0a262f456fe230d8308ea4d61d65a |}]
      ;;

      let to_model t = t
    end
    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t =
        { tip              : Rev.V1.t
        ; remote_repo_path : Remote_repo_path.V1.t
        }
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 002e3541a7b3b8f948dc74fada75b43e |}]
      ;;

      let of_model t = t
    end
    module Model = V1
  end
end

include Iron_command_rpc.Make
    (struct let name = "wait-for-hydra" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action = Stable.Action.     Model
module Reaction = Stable.Reaction. Model
