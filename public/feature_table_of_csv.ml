module Stable = struct
  open! Import_stable

  module Row = struct
    module V1 = struct
      type 'a t =
        { feature_path  : 'a
        ; other_columns : string array
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: Bin_digest_type_variable.tick_a t];
        [%expect {| 4d0276253ee912d2880704aa70e3ce93 |}]
      ;;
    end

    module Model = V1
  end

  module Action = struct
    module V1 = struct
      type t =
        { headers                 : string Row.V1.t
        ; data                    : Feature_path.V1.t Row.V1.t list
        ; preserve_input_ordering : bool
        ; display_ascii           : bool
        ; max_output_columns      : int
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8f5e5acb0ffb376da2d11a28cc68da79 |}]
      ;;

      let to_model m = m
    end

    module Model = V1
  end

  module Reaction = struct
    module V1 = struct
      type t = string [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
      ;;

      let of_model m = m
    end

    module Model = V1
  end
end

include Iron_command_rpc.Make
    (struct let name = "feature-table-of-csv" end)
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
module Row      = Stable.Row.      Model
