module type S = sig
  open! Core
  open! Import

  type t [@@deriving sexp_of]

  include Comparable  with type t := t
  include Hashable    with type t := t
  include Invariant.S with type t := t
  include Stringable  with type t := t

  val create : unit -> t

  val to_file_name : t -> File_name.t

  val length_of_string_repr : int

  module Stable : sig
    module V1 : Stable_without_comparator with type t = t
  end
end
