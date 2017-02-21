open! Core
open! Async
open! Import

type t [@@deriving sexp_of]

val name : t -> string

module Compare_by_name_and_version : sig
  type nonrec t = t [@@deriving compare]
end

val create
  :  name     : string
  -> version  : int
  -> query    : Bin_prot.Shape.t
  -> response : Bin_prot.Shape.t
  -> t

val to_ascii_table : t list -> Iron_ascii_table.t

module Stable : sig
  module V2 : Stable_without_comparator with type t = t
  module V1 : sig
    include Stable_without_comparator with type t = Rpc.Description.t

    val of_v2 : V2.t -> t
  end
end
