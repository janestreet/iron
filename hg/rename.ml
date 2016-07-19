module Stable = struct
  open! Core.Stable
  open! Import_stable

  module V2 = struct
    type t =
      { feature_id : Feature_id.V1.t
      ; from       : Feature_path.V1.t
      ; to_        : Feature_path.V1.t
      }
    [@@deriving bin_io, compare, sexp]
  end

  module V1 = struct
    type t =
      { from : Feature_path.V1.t
      ; to_  : Feature_path.V1.t
      }
    [@@deriving bin_io]

    let of_v2 { V2.feature_id = _; from; to_ } = { from; to_ }
  end
end

include Stable.V2
