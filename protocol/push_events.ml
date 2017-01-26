module Stable = struct
  open! Import_stable

  module Add = struct
    module Action = struct
      module V1 = struct
        type t =
          { feature_id : Feature_id.V1.t
          ; tip        : Rev.V1.t
          }
        [@@deriving bin_io, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 2ac410907e9893149d0fd4a7f5d76e43 |}]
        ;;

        let to_model m = m
      end

      module Model = V1
    end

    module Reaction = struct
      module V1 = Unit
      module Model = V1
    end
  end

  module Change = struct
    module Action = struct
      module V1 = struct
        type t =
          | Clear_all
          | Clear_features           of Feature_id.V1.t list
          | Clear_revs               of Rev.V1.t list
          | Clear_users              of User_name.V1.t list
          | Set_max_size_per_feature of int
        [@@deriving bin_io, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 1cb3cdefa407a6238f83ac1fc8f98944 |}]
        ;;

        let to_model m = m
      end

      module Model = V1
    end

    module Reaction = struct
      module V1 = Unit
      module Model = V1
    end
  end
end

open! Core
open! Import

module What_to_dump = struct
  type t =
    [ `Feature_id of Feature_id.t
    | `Stats
    | `User_name of User_name.t
    | `Values
    ]
  [@@deriving sexp_of]

  let require_admin_privileges = function
    | `Values -> true
    | `Feature_id _ | `Stats | `User_name _ -> false
  ;;
end

module Add = struct
  module Stable = Stable.Add

  include Iron_versioned_rpc.Make
      (struct let name = "push-event-add" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)

  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end

module Change = struct
  module Stable = Stable.Change

  include Iron_versioned_rpc.Make
      (struct let name = "push-event-change" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)

  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end
