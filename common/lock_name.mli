open! Core.Std
open! Import

type t =
  | Rebase
  | Release
  | Release_into
  | Rename
[@@deriving enumerate, sexp_of] (* enumerate is for the command-line flags *)

include Comparable with type t := t
include Hashable   with type t := t

val to_string_hum : t -> string

module Stable : sig
  module Model : T with type t = t

  module V2 : sig
    include Stable_without_comparator with type t = Model.t
  end

  module V1 : sig
    include Stable_without_comparator
    val of_v2    : V2.t -> t option
    val to_v2    : t -> V2.t
  end
end
