module Table = struct
  module Stable = struct

    open! Import_stable

    module Action = struct
      module V1 = struct
        type t =
          { features                : Iron_protocol.List_features.Reaction.Stable.V8.t
          ; preserve_input_ordering : bool
          ; display_ascii           : bool
          ; max_output_columns      : int
          }
        [@@deriving bin_io, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| b76a51ced87216fd0de6a05d4b7717d2 |}]
        ;;

        let to_model m = m
      end

      module Model = V1
    end

    module Reaction = struct
      module V1 = struct
        type t = string
        [@@deriving bin_io, sexp_of]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| d9a8da25d5656b016fb4dbdc2e4197fb |}]
        ;;
        let of_model t = t
      end

      module Model = V1
    end
  end

  include Iron_command_rpc.Make
      (struct let name = "list-table" end)
      (struct let version = 1 end)
      (Stable.Action.V1)
      (Stable.Reaction.V1)

  module Action   = Stable.Action.   Model
  module Reaction = Stable.Reaction. Model
end
