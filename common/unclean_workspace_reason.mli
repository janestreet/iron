open! Core
open! Import

type t [@@deriving compare, sexp_of]

include Invariant.S with type t := t
include Equal.S     with type t := t

val to_string_hum : t -> string

val to_ascii_table_column_text : t -> string

module One_reason : sig
  type t =
    | Error of Error.t
    | Invalid_current_bookmark
    | Pending_rename
    | Shelved_changes
    | Uncommitted_changes
    | Unpushed_changesets
  [@@deriving sexp_of]
end

val create : One_reason.t list -> t option

val error : Error.t -> t

val add : t -> t -> t

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
