module Stable = struct
  open! Core.Core_stable
  open! Import_stable

  module Namespace = struct
    module V4 = struct
      type t =
        [ `All
        | `Archived
        | `Existing
        | `Existing_or_most_recently_archived
        | `Existing_or_with_catch_up
        ]
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 36d03ee253445ec0aacab830e43e0d44 |}]
      ;;
    end
    module V2 = struct
      type t =
        [ `All
        | `Archived
        | `Existing
        | `Existing_or_most_recently_archived
        ]
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| f24447e5f46ea9ce5b46eced3ef6a31c |}]
      ;;

      let to_v4 (t : t) : V4.t = (t :> V4.t)
    end
    module V1 = struct
      type t =
        [ `Archived
        | `Existing
        | `All
        ]
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 32aa3dcc5c94e189dbf61fc73ba0549c |}]
      ;;

      let to_v2 (t : t) : V2.t = (t :> V2.t)
    end
    module Model = V4
  end

  module Feature_spec = struct
    module V1 = struct
      type t =
        [ `Feature_id   of Feature_id.V1.t
        | `Feature_path of Feature_path.V1.t
        ]
      [@@deriving bin_io, compare, sexp]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 19f73b218581759912f73390d7e1d813 |}]
      ;;
    end
    module Model = V1
  end

  module V3 = struct
    type t =
      { feature_spec : Feature_spec.V1.t
      ; namespace    : Namespace.V4.t
      }
    [@@deriving bin_io, compare, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 4164209ba45744542ce9faf3844df40e |}]
    ;;
  end

  module V2 = struct
    type t =
      { feature_spec : Feature_spec.V1.t
      ; namespace    : Namespace.V2.t
      }
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| bad769765b762926f3053429c38bf52b |}]
    ;;

    let to_v3 { feature_spec; namespace } =
      { V3.
        feature_spec
      ; namespace    = Namespace.V2.to_v4 namespace
      }
    ;;
  end

  module V1 = struct
    type t =
      { feature_spec : Feature_spec.V1.t
      ; namespace    : Namespace.V1.t
      }
    [@@deriving bin_io]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| cf83d33ba9da1caac7ec0a230194119c |}]
    ;;

    let existing_feature_path feature_path =
      { feature_spec = `Feature_path feature_path
      ; namespace    = `Existing
      }
    ;;

    let to_v2 { feature_spec; namespace } =
      { V2.
        feature_spec
      ; namespace    = Namespace.V1.to_v2 namespace
      }
    ;;
  end

  module Model = V3
end

open! Core
open! Import

module Namespace = Stable.Namespace.Model
module Feature_spec = Stable.Feature_spec.Model
include Stable.Model

module Command_line = struct

  module Feature_spec = struct
    type t =
      [ `Feature_id   of Feature_id.t
      | `Feature_path of Feature_path.t
      | `Partial_name of string
      | `Current_bookmark
      ]
    [@@deriving sexp_of]
  end

  type t =
    { feature_spec : Feature_spec.t
    ; namespace    : Namespace.t
    }
  [@@deriving sexp_of]
end
