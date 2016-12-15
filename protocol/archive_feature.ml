module Stable = struct

  open Import_stable

  module Action = struct
    module V2 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; rev_zero     : Rev.V1.t
        ; for_         : User_name.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 32b2a30be1ac1a96c251f3b7e5b5366a |}]
      ;;

      let to_model t = t
    end
  end

  module Reaction = struct
    module V2 = struct
      type t =
        { remote_repo_path : Remote_repo_path.V1.t
        ; send_email_to    : Email_address.V1.Set.t
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 9afe00a7e27c6fa10e2f40666a285ee8 |}]
      ;;

      let of_model t = t
    end

    module Model = V2

    module V1 = struct
      type t =
        { remote_repo_path      : Remote_repo_path.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f02e28f8234664a56eb29c60d2f3d589 |}]
      ;;

      let of_model { Model. remote_repo_path; _ } = { remote_repo_path }
    end
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "archive-feature" end)
    (struct let version = 3 end)
    (Stable.Action.V2)
    (Stable.Reaction.V2)

include Register_old_rpc
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

module Action   = Stable.Action.V2
module Reaction = Stable.Reaction.V2
