open! Core
open! Import

type t =
  | Create_child
  | Rebase
  | Release
  | Release_into
  | Rename
  | Second
[@@deriving enumerate, sexp_of]

include Comparable.S_plain with type t := t
include Hashable.  S_plain with type t := t

val to_string_hum : t -> string

module Stable : sig
  module Model : T with type t = t

  module V3 : sig
    include Stable_without_comparator with type t = Model.t
  end

  module V2 : sig
    include Stable_without_comparator
    val of_v3    : V3.t -> t option
    val to_v3    : t -> V3.t
  end

  module V1 : sig
    include Stable_without_comparator
    val of_v2    : V2.t -> t option
    val to_v2    : t -> V2.t
  end
end
