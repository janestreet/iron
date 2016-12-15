module Stable = struct

  open Import_stable

  module Action = struct
    module V2 = struct
      type t =
        { descendants_of : Which_ancestor.V1.t
        ; depth          : int
        ; use_archived   : bool
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 02b4e4907b5b0762795ecc5e35b9aa90 |}]
      ;;

      let to_model (t : t) = t
    end

    module V1 = struct
      type t =
        { feature_path : Feature_path.V1.t option
        ; depth        : int
        ; use_archived : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 11927d8da4c4c4acd888bf95dc258516 |}]
      ;;

      let to_model { feature_path
                   ; depth
                   ; use_archived
                   } =
        let descendants_of =
          match feature_path with
          | None -> Which_ancestor.V1.Any_root
          | Some feature -> Feature feature
        in
        V2.to_model
          { descendants_of
          ; depth
          ; use_archived
          }
      ;;
    end

    module Model = V2
  end

  module Reaction = struct
    module V8 = struct
      type one =
        { feature_path      : Feature_path.V1.t
        ; feature_id        : Feature_id.V1.t
        ; owners            : User_name.V1.t list
        ; review_is_enabled : bool
        ; num_lines         : int Or_error.V1.t Or_pending.V1.t
        ; next_steps        : Next_step.V5.t list
        ; status            : [ `Existing
                              | `Was_archived_at of Time.V1_round_trippable.t
                              ]
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: one];
        [%expect {| 4acccdbd1a3f3d12e25ec21b473a399d |}]
      ;;

      type t = one list
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f19947ea15a45be8d3610debd391ee88 |}]
      ;;

      let of_model m = m
    end

    module V7 = struct
      type one =
        { feature_path      : Feature_path.V1.t
        ; review_is_enabled : bool
        ; num_lines         : int Or_error.V1.t Or_pending.V1.t
        ; next_steps        : Next_step.V5.t list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: one];
        [%expect {| c64edd39e2a49fbfb397d8bb88535d29 |}]
      ;;

      type t = one list
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 2adbe7ee5e2407dd60d57b244d52e47f |}]
      ;;

      let of_model m =
        List.map m ~f:(fun m ->
          let { V8.
                feature_path
              ; review_is_enabled
              ; num_lines
              ; next_steps
              ; _
              } = V8.of_model m in
          { feature_path
          ; review_is_enabled
          ; num_lines
          ; next_steps
          })
      ;;
    end

    module Model = V8
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "list-features" end)
    (struct let version = 9 end)
    (Stable.Action.V2)
    (Stable.Reaction.V8)

include Register_old_rpc
    (struct let version = 8 end)
    (Stable.Action.V1)
    (Stable.Reaction.V8)

include Register_old_rpc
    (struct let version = 7 end)
    (Stable.Action.V1)
    (Stable.Reaction.V7)

module Action   = Stable.Action.Model

module Reaction = struct
  module Stable = Stable.Reaction
  include Stable.Model
end
