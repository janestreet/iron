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

  module V1 : sig
    include Stable_without_comparator with type t = Model.t
    val of_model : Model.t -> t
  end
end
