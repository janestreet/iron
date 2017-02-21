open! Core
open! Import

type t =
  { feature_path : Feature_path.t
  ; reason       : Unclean_workspace_reason.t
  }
[@@deriving compare, fields, sexp_of]

include Invariant.S with type t := t

module Stable : sig
  module Model : T with type t = t

  module V2 : sig
    include Stable_without_comparator with type t = Model.t
  end

  module V1 : sig
    include Stable_without_comparator
    val of_v2 : V2.t -> t
    val to_v2 : t -> V2.t
  end
end
