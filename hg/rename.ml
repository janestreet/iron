module Stable = struct
  open! Core.Stable
  open! Import_stable

  module V1 = struct
    type t =
      { from : Feature_path.V1.t
      ; to_  : Feature_path.V1.t
      }
    [@@deriving bin_io, compare, sexp]
  end
end

include Stable.V1
