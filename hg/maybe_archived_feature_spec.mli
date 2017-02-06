open! Core
open! Async
open! Import

module Namespace : sig
  type t =
    [ `All
    | `Archived
    | `Existing
    | `Existing_or_most_recently_archived
    | `Existing_or_with_catch_up
    ]
  [@@deriving sexp_of]
end

module Feature_spec : sig
  type t =
    [ `Feature_id   of Feature_id.t
    | `Feature_path of Feature_path.t
    ]
  [@@deriving sexp_of]
end

type t =
  { feature_spec : Feature_spec.t
  ; namespace    : Namespace.t
  }
[@@deriving sexp_of]

module Command_line : sig

  module Feature_spec : sig
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

module Stable : sig
  module V3 : Stable_without_comparator with type t = t
  module V2 : sig
    type t [@@deriving bin_io]
    val to_v3 : t -> V3.t
  end
  module V1 : sig
    type t [@@deriving bin_io]
    val existing_feature_path : Feature_path.t -> t
    val to_v2 : t -> V2.t
  end
  module Namespace : sig
    module V4 : Stable_without_comparator with type t = Namespace.t
    module V2 : sig
      type t =
        [ `All
        | `Archived
        | `Existing
        | `Existing_or_most_recently_archived
        ]
      [@@deriving bin_io]
      val to_v4 : t -> V4.t
    end
    module V1 : sig
      type t [@@deriving bin_io]
      val to_v2 : t -> V2.t
    end
  end
end
