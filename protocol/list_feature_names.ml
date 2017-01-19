module Stable = struct

  open! Import_stable

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
    module V1 = struct
      type t = Feature_path.V1.t list
      [@@deriving bin_io, sexp_of]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| e16050fbbb13a3a25cae42a413f70535 |}]
      ;;

      let of_model t = t
    end

    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "list-feature-names" end)
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action   = Stable.Action.   Model
module Reaction = Stable.Reaction. Model
