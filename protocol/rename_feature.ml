module Stable = struct

  open! Import_stable

  module Action = struct
    module V3 = struct
      type t =
        { from           : Feature_path.V1.t
        ; to_            : Feature_path.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ef36ab97563f6e9376fed8899f0d88b3 |}]
      ;;

      let to_model t = t
    end

    module V2 = struct
      type t =
        { from           : Feature_path.V1.t
        ; to_            : Feature_path.V1.t
        ; even_if_locked : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| cdb611f0994e10341cb00b3991bef59d |}]
      ;;

      let to_model { from; to_; even_if_locked = _ } =
        V3.to_model { V3.from; to_ }
      ;;
    end

    module V1 = struct
      type t =
        { from : Feature_path.V1.t
        ; to_  : Feature_path.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| ef36ab97563f6e9376fed8899f0d88b3 |}]
      ;;

      let to_model { from; to_ } =
        V3.to_model { V3. from; to_ }
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
    (struct let name = "rename-feature" end)
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
