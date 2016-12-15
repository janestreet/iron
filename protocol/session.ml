module Stable = struct

  open! Import_stable

  module Commit = struct
    module Action = struct
      module V1 = struct
        type t =
          { feature_path      : Feature_path.V1.t
          ; for_              : User_name.V1.t
          ; review_session_id : Session_id.V1.t
          }
        [@@deriving bin_io, fields, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| bf7dd9b78cb08c3f8dd2c32369944dd8 |}]
        ;;

        let to_model t = t
      end

      module Model = V1
    end

    module Reaction = struct
      module V1 = Unit
      module Model = V1
    end
  end

  module Forget = struct
    module Action = struct
      module V1 = struct
        type t =
          { feature_path      : Feature_path.V1.t
          ; for_              : User_name.V1.t
          ; review_session_id : Session_id.V1.t
          ; what_to_forget    : [ `All
                                | `Files of Path_in_repo.V1.t list
                                ]
          }
        [@@deriving bin_io, fields, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 57a744efb86415ba24c4df79839890f6 |}]
        ;;

        let to_model t = t
      end

      module Model = V1
    end

    module Reaction = struct
      module V1 = Unit
      module Model = V1
    end
  end

  module Set_lock = struct
    module Action = struct
      module V1 = struct
        type t =
          { feature_path     : Feature_path.V1.t
          ; for_             : User_name.V1.t
          ; which_session    : Which_session.V1.t
          ; set_is_locked_to : bool
          }
        [@@deriving bin_io, fields, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 4e5885e58609f12b25adf343d36a1a1e |}]
        ;;

        let to_model t = t
      end

      module Model = V1
    end

    module Reaction = struct
      module V1 = Unit
      module Model = V1
    end
  end
end

module Commit = struct
  module Stable = Stable.Commit
  include Iron_versioned_rpc.Make
      (struct let name = "commit-session" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)
  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end

module Forget = struct
  module Stable = Stable.Forget
  include Iron_versioned_rpc.Make
      (struct let name = "forget-session" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)
  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end

module Set_lock = struct
  module Stable = Stable.Set_lock
  include Iron_versioned_rpc.Make
      (struct let name = "set-lock-session" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)
  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end
