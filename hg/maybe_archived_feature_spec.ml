module Stable = struct
  open! Core.Stable
  open! Import_stable

  module Namespace = struct
    module V2 = struct
      type t =
        [ `All
        | `Archived
        | `Existing
        | `Existing_or_most_recently_archived
        ]
      [@@deriving bin_io, compare, sexp]
    end
    module V1 = struct
      type t =
        [ `Archived
        | `Existing
        | `All
        ]
      [@@deriving bin_io]

      let to_v2 (t : t) : V2.t = (t :> V2.t)
    end
    module Model = V2
  end

  module Feature_spec = struct
    module V1 = struct
      type t =
        [ `Feature_id   of Feature_id.V1.t
        | `Feature_path of Feature_path.V1.t
        ]
      [@@deriving bin_io, compare, sexp]
    end
    module Model = V1
  end

  module V2 = struct
    type t =
      { feature_spec : Feature_spec.V1.t
      ; namespace    : Namespace.V2.t
      }
    [@@deriving bin_io, compare, sexp]
  end

  module V1 = struct
    type t =
      { feature_spec : Feature_spec.V1.t
      ; namespace    : Namespace.V1.t
      }
    [@@deriving bin_io]

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

  module Model = V2
end

open! Core.Std
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
