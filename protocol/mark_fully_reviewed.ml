module Stable = struct

  open! Import_stable

  module Action = struct
    module V7 = struct
      type t =
        { feature_path           : Feature_path.V1.t
        ; whom_to_mark           : User_name.Or_all_or_all_but.V1.t
        ; reason                 : string
        ; create_catch_up_for_me : bool
        ; base                   : Rev.V1.t option
        ; tip                    : Rev.V1.t option
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| b16d1428cbd15e22f7657d7451d0cdb6 |}]
      ;;

      let to_model (t : t) = t
    end

    module V6 = struct
      type t =
        { feature_path           : Feature_path.V1.t
        ; for_or_all             : User_name.Or_all.V1.t
        ; reason                 : string
        ; create_catch_up_for_me : bool
        ; base                   : Rev.V1.t option
        ; tip                    : Rev.V1.t option
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 21b046251d98de455cef9f931fbc79d0 |}]
      ;;

      let to_model { feature_path
                   ; for_or_all
                   ; reason
                   ; create_catch_up_for_me
                   ; base
                   ; tip
                   } =
        V7.to_model
          { feature_path
          ; whom_to_mark = (for_or_all :> User_name.Or_all_or_all_but.V1.t)
          ; reason
          ; create_catch_up_for_me
          ; base
          ; tip
          }
      ;;

    end

    module V5 = struct
      type t =
        { feature_path    : Feature_path.V1.t
        ; for_or_all      : User_name.Or_all.V1.t
        ; reason          : string
        ; base            : Rev.V1.t option
        ; tip             : Rev.V1.t option
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 5d0f9db3897c614ed0d12b2a97c42f1a |}]
      ;;

      let to_model
            { feature_path
            ; for_or_all
            ; reason
            ; base
            ; tip
            } =
        V6.to_model
          { feature_path
          ; for_or_all
          ; reason
          ; create_catch_up_for_me = false
          ; base
          ; tip
          }
      ;;
    end

    module V4 = struct
      type t =
        { feature_path : Feature_path.V1.t
        ; for_or_all   : User_name.Or_all.V1.t
        ; reason       : string
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 6e44c961df0fc0cdc26adec8a5706275 |}]
      ;;

      let to_model { feature_path; for_or_all; reason } =
        V5.to_model
          { feature_path
          ; for_or_all
          ; reason
          ; base         = None
          ; tip          = None
          }
      ;;
    end
    module Model = V7
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "mark-fully-reviewed" end)
    (struct let version = 7 end)
    (Stable.Action.V7)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 6 end)
    (Stable.Action.V6)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 5 end)
    (Stable.Action.V5)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 4 end)
    (Stable.Action.V4)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
