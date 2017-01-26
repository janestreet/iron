module Stable = struct
  open! Import_stable

  module Action = struct
    module V3 = struct
      type t =
        { feature_path   : Feature_path.V1.t
        ; for_           : User_name.V1.t
        ; rev_zero       : Rev.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| e14aed158f40dd9d2daec8aa6a89634c |}]
      ;;

      let to_model t = t
    end

    module V2 = struct
      type t =
        { feature_path   : Feature_path.V1.t
        ; for_           : User_name.V1.t
        ; even_if_locked : bool
        ; rev_zero       : Rev.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 070c78eca771374ec9be10d8928f5bf4 |}]
      ;;

      let to_model { feature_path
                   ; for_
                   ; even_if_locked = _
                   ; rev_zero
                   } =
        V3.to_model
          { V3.
            feature_path
          ; for_
          ; rev_zero
          }
      ;;
    end

    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; for_         : User_name.V1.t
        ; rev_zero     : Rev.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| e14aed158f40dd9d2daec8aa6a89634c |}]
      ;;

      let to_model { feature_path
                   ; for_
                   ; rev_zero
                   } =
        V3.to_model
          { V3.
            feature_path
          ; for_
          ; rev_zero
          }
      ;;
    end

    module Model = V3
  end

  module Reaction = struct
    module V2 = struct
      type t =
        { feature_tip      : Rev.V1.t
        ; parent_tip       : Rev.V1.t
        ; renames          : Rename.V2.t list
        ; remote_repo_path : Remote_repo_path.V1.t
        }
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 5c1ee3ae2701a7849e8c6974a6714330 |}]
      ;;

      let of_model (t : t) = t
    end

    module V1 = struct
      type t =
        { feature_tip      : Rev.V1.t
        ; parent_tip       : Rev.V1.t
        ; renames          : Rename.V1.t list
        ; remote_repo_path : Remote_repo_path.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 7fa10f8d6349efbbeb016d3c1a84266d |}]
      ;;

      open! Core
      open! Import

      let of_model m =
        let { V2.
              feature_tip
            ; parent_tip
            ; renames
            ; remote_repo_path
            } = V2.of_model m in
        { feature_tip
        ; parent_tip
        ; renames          = List.map renames ~f:Rename.Stable.V1.of_v2
        ; remote_repo_path
        }
      ;;
    end

    module Model = V2
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "prepare-to-compress" end)
    (struct let version = 4 end)
    (Stable.Action.V3)
    (Stable.Reaction.V2)

include Register_old_rpc
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
