module Stable = struct
  open Core.Stable
  open Import_stable
  module Reason = struct
    module V1 = struct
      type t =
        | External of unit Query.V1.t
        | Release of Feature_path.V1.t * Rev.V1.t
        | Review
      [@@deriving bin_io, compare, sexp]
    end
  end

  module V1 = struct
    type t =
      { rev_zero : Rev.V1.t
      ; from_    : Rev.V1.t
      ; to_      : Rev.V1.t
      ; reason   : Reason.V1.t
      }
    [@@deriving bin_io, compare, fields, sexp]
  end
end

open! Core.Std
open! Import

include Stable.V1

module Reason = Stable.Reason.V1

let invariant (_ : t) = ()
