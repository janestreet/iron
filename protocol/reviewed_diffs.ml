module Stable = struct

  open! Import_stable

  module Action = struct
    module V4 = struct
      type t =
        { feature_path                            : Feature_path.V1.t
        ; for_                                    : User_name.V1.t
        ; reason                                  : string
        ; create_catch_up_for_me                  : bool
        ; even_if_some_files_are_already_reviewed : bool
        ; review_session_id                       : Session_id.V1.t
        ; diff4_in_session_ids                    : Diff4_in_session.Id.V1.t list
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f3335f86d30525788ef36c01b85fdc95 |}]
      ;;

      let to_model (t : t) = t
    end

    module V3 = struct
      type t =
        { feature_path           : Feature_path.V1.t
        ; for_                   : User_name.V1.t
        ; reason                 : string
        ; create_catch_up_for_me : bool
        ; review_session_id      : Session_id.V1.t
        ; diff4_in_session_ids   : Diff4_in_session.Id.V1.t list
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| d6ffce44ac8dcc57c71daf9aa47dbd99 |}]
      ;;

      let to_model { feature_path
                   ; for_
                   ; reason
                   ; create_catch_up_for_me
                   ; review_session_id
                   ; diff4_in_session_ids
                   } =
        V4.to_model
          { feature_path
          ; for_
          ; reason
          ; create_catch_up_for_me
          ; even_if_some_files_are_already_reviewed = false
          ; review_session_id
          ; diff4_in_session_ids
          }
      ;;
    end

    module V2 = struct
      type t =
        { feature_path         : Feature_path.V1.t
        ; for_                 : User_name.V1.t
        ; reason               : string
        ; review_session_id    : Session_id.V1.t
        ; diff4_in_session_ids : Diff4_in_session.Id.V1.t list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 2d862dad9c8e60668f40f2cce4cc312f |}]
      ;;

      let to_model { feature_path
                   ; for_
                   ; reason
                   ; review_session_id
                   ; diff4_in_session_ids
                   } =
        V3.to_model
          { feature_path
          ; for_
          ; reason
          ; create_catch_up_for_me = false
          ; review_session_id
          ; diff4_in_session_ids
          }
      ;;
    end

    module Model = V4
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "reviewed-diff4" end)
    (struct let version = 4 end)
    (Stable.Action.V4)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 3 end)
    (Stable.Action.V3)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
