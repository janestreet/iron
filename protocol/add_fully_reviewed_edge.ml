module Stable = struct

  open! Import_stable

  module Feature_base_to_tip = struct
    module V3 = struct
      type t =
        { feature_path               : Feature_path.V1.t
        ; even_if_release_is_locked  : bool
        ; if_feature_is_empty        : [ `Do_nothing | `Fail ]
        }
      [@@deriving bin_io, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8c2363cf4597ea6e03d093f5df34a241 |}]
      ;;
    end

    module V2 = struct
      type t =
        { feature_path              : Feature_path.V1.t
        ; even_if_release_is_locked : bool
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| abed2c67525d2e2725b3a40f53778462 |}]
      ;;

      let to_v3 { feature_path; even_if_release_is_locked } =
        { V3.
          feature_path
        ; even_if_release_is_locked
        ; if_feature_is_empty       = `Fail
        }
      ;;
    end

    module V1 = struct
      type t = Feature_path.V1.t
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 296be80010ace497614f92952e5510c4 |}]
      ;;

      let to_v2 t =
        { V2.
          feature_path              = t
        ; even_if_release_is_locked = false
        }
      ;;
    end

    module Model = V3
  end

  module Action = struct
    module V3 = struct
      type t =
        { rev_zero : Rev.V1.t
        ; edge     : [ `Feature_base_to_tip of Feature_base_to_tip.V3.t
                     | `From_to             of Rev.V1.t * Rev.V1.t
                     ]
        }
      [@@deriving bin_io, fields, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| e7a6d2d334e3c5f12f3207948f97a73a |}]
      ;;

      let to_model (t : t) = t
    end

    module V2 = struct
      type t =
        { rev_zero : Rev.V1.t
        ; edge     : [ `Feature_base_to_tip of Feature_base_to_tip.V2.t
                     | `From_to             of Rev.V1.t * Rev.V1.t
                     ]
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 2270faa8041878b4c631ef9b27e77047 |}]
      ;;

      let to_model { rev_zero; edge } =
        let edge =
          match edge with
          | `From_to _ as e -> e
          | `Feature_base_to_tip v ->
            `Feature_base_to_tip (Feature_base_to_tip.V2.to_v3 v)
        in
        V3.to_model { rev_zero; edge }
      ;;
    end

    module V1 = struct
      type t =
        { rev_zero : Rev.V1.t
        ; edge     : [ `Feature_base_to_tip of Feature_base_to_tip.V1.t
                     | `From_to             of Rev.V1.t * Rev.V1.t
                     ]
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 2fab2225d2bb557e0d70b75511015ed1 |}]
      ;;

      let to_model { rev_zero; edge } =
        let edge =
          match edge with
          | `From_to _ as from_to -> from_to
          | `Feature_base_to_tip v1 ->
            `Feature_base_to_tip (Feature_base_to_tip.V1.to_v2 v1)
        in
        V2.to_model { V2. rev_zero; edge }
      ;;
    end

    module Model = V3
  end

  module Reaction = struct
    module V1 = Unit
    module Model = V1
  end
end

include Iron_versioned_rpc.Make
    (struct let name = "add-fully-reviewed-edge" end)
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

module Action              = Stable.Action.              Model
module Feature_base_to_tip = Stable.Feature_base_to_tip. Model
module Reaction            = Stable.Reaction.            Model
