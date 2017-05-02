module Stable = struct

  open! Import_stable

  module Action = struct
    module V3 = struct
      type t =
        { feature_path         : Feature_path.V1.t
        ; rev_zero             : Rev.V1.t
        ; for_                 : User_name.V1.t
        ; reason_for_archiving : string
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| cc9a8aebc131bc83c591eaf916528aa7 |}]
      ;;

      let to_model (t : t) = t
    end

    module V2 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; rev_zero     : Rev.V1.t
        ; for_         : User_name.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 32b2a30be1ac1a96c251f3b7e5b5366a |}]
      ;;

      let to_model { feature_path
                   ; rev_zero
                   ; for_
                   } =
        V3.to_model
          { feature_path
          ; rev_zero
          ; for_
          ; reason_for_archiving = ""
          }
      ;;
    end

    module Model = V3
  end

  module Reaction = struct
    module V2 = struct
      type t =
        { remote_repo_path : Remote_repo_path.V1.t
        ; send_email_to    : Email_address.V1.Set.t
        }
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 9afe00a7e27c6fa10e2f40666a285ee8 |}]
      ;;

      let of_model (t : t) = t
    end

    module V1 = struct
      type t =
        { remote_repo_path      : Remote_repo_path.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f02e28f8234664a56eb29c60d2f3d589 |}]
      ;;

      let of_model m =
        let { V2. remote_repo_path; _ } = V2.of_model m in
        { remote_repo_path }
      ;;
    end

    module Model = V2
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "archive-feature" end)
    (struct let version = 4 end)
    (Stable.Action.V3)
    (Stable.Reaction.V2)

include Register_old_rpc
    (struct let version = 3 end)
    (Stable.Action.V2)
    (Stable.Reaction.V2)

include Register_old_rpc
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
