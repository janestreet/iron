module Table = struct
  module Stable = struct

    open! Import_stable

    module Action = struct
      module V3 = struct
        type t =
          { features                : Iron_protocol.List_features.Reaction.Stable.V10.t
          ; preserve_input_ordering : bool
          ; display_ascii           : bool
          ; max_output_columns      : int
          }
        [@@deriving bin_io, sexp]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| d248970c6a3a95998a29eed335a3b8b8 |}]
        ;;

        let to_model (m : t) = m
      end

      module V2 = struct
        type t =
          { features                : Iron_protocol.List_features.Reaction.Stable.V9.t
          ; preserve_input_ordering : bool
          ; display_ascii           : bool
          ; max_output_columns      : int
          }
        [@@deriving bin_io]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 69fe1854488a4271239720e4577d614f |}]
        ;;

        let to_model { features
                     ; preserve_input_ordering
                     ; display_ascii
                     ; max_output_columns
                     } =
          let features = Iron_protocol.List_features.Reaction.Stable.V9.to_v10 features in
          V3.to_model
            { features
            ; preserve_input_ordering
            ; display_ascii
            ; max_output_columns
            }
        ;;
      end

      module V1 = struct
        type t =
          { features                : Iron_protocol.List_features.Reaction.Stable.V8.t
          ; preserve_input_ordering : bool
          ; display_ascii           : bool
          ; max_output_columns      : int
          }
        [@@deriving bin_io]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| b76a51ced87216fd0de6a05d4b7717d2 |}]
        ;;

        let to_model { features
                     ; preserve_input_ordering
                     ; display_ascii
                     ; max_output_columns
                     } =
          let features = Iron_protocol.List_features.Reaction.Stable.V8.to_v9 features in
          V2.to_model
            { features
            ; preserve_input_ordering
            ; display_ascii
            ; max_output_columns
            }
        ;;
      end

      module Model = V3
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
end
