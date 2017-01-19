module Stable = struct

  open! Import_stable

  module Included_features_order = struct
    module V1 = struct
      type t =
        [ `Name
        | `Release_order
        ]
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8657555ed2892969e81c717ea5c480ca |}]
      ;;
    end

    module Model = V1
  end

  module Action = struct

    module V2 = struct
      type t =
        { feature_path            : Feature_path.V1.t
        ; for_                    : User_name.V1.t
        ; included_features_order : Iron_protocol.Feature.Stable.Sorted_by.V2.t
        }
      [@@deriving bin_io, sexp]

      let to_model (m : t) = m

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| d23bb8a12178989a96c96b68f92e0420 |}]
      ;;
    end

    module V1 = struct
      type t =
        { feature_path            : Feature_path.V1.t
        ; for_                    : User_name.V1.t
        ; included_features_order : Iron_protocol.Feature.Stable.Sorted_by.V1.t
        }
      [@@deriving bin_io]

      let to_model { feature_path; for_; included_features_order; } =
        V2.to_model
          { feature_path
          ; for_
          ; included_features_order =
              Iron_protocol.Feature.Stable.Sorted_by.V1.to_v2 included_features_order
          }
      ;;

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 796b5bc89af21d8dbb38574a39e64011 |}]
      ;;
    end

    module Model = V2
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

include Iron_command_rpc.Make
    (struct let name = "release" end)
    (struct let version = 2 end)
    (Stable.Action.V2)
    (Stable.Reaction.V1)

include Register_old_rpc
    (struct let version = 1 end)
    (Stable.Action.V1)
    (Stable.Reaction.V1)

module Action                  = Stable.Action.                  Model
module Included_features_order = Stable.Included_features_order. Model
module Reaction                = Stable.Reaction.                Model
