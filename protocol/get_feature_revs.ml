module Stable = struct

  open! Import_stable

  module Feature = Feature.Stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; rev_zero     : Rev.V1.t option
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b2b386963682bcb97c2bb87f82e9e4e4 |}]
      ;;

      let to_model t = t
    end
    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t =
        { base             : Rev.V1.t
        ; tip              : Rev.V1.t
        ; remote_repo_path : Remote_repo_path.V1.t
        }
      [@@deriving bin_io, compare, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 3feb870f58bd262522c68297d4a85237 |}]
      ;;

      let of_model t = t
    end
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "get-feature-revs" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
