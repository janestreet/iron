module Stable = struct

  open! Import_stable

  module Action = struct
    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 82e9dd83ba682394b6a7532a1bdf9e67 |}]
      ;;

      let to_model t = t
    end

    module Model = V1
  end

  module Reaction = struct
    module V2 = struct
      type t =
        { de_aliased                                         : User_name.V1.Set.t
        ; did_not_de_alias_due_to_review_session_in_progress : User_name.V1.Set.t
        ; nothing_to_do                                      : User_name.V1.Set.t
        }
      [@@deriving bin_io, compare, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 3b77e4555fdf4bf50e45fc1d49cc5140 |}]
      ;;

      let of_model (t : t) = t
    end

    module V1 = struct
      type t =
        { de_aliased                                : User_name.V1.Set.t
        ; did_not_de_alias_due_to_non_empty_session : User_name.V1.Set.t
        ; nothing_to_do                             : User_name.V1.Set.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 1900375ddbc456393c90b406b0379942 |}]
      ;;

      let of_model m =
        let { V2.
              de_aliased
            ; did_not_de_alias_due_to_review_session_in_progress
            ; nothing_to_do
            } = V2.of_model m
        in
        let did_not_de_alias_due_to_non_empty_session =
          did_not_de_alias_due_to_review_session_in_progress
        in
        { de_aliased
        ; did_not_de_alias_due_to_non_empty_session
        ; nothing_to_do
        }
      ;;
    end

    module Model = V2
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "de-alias-feature" end)
    (struct let version = 2 end)
    (Stable.Action.V1)
    (Stable.Reaction.V2)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
