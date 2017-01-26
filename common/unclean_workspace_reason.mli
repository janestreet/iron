open! Core
open! Import

type one_reason =
  | Uncommitted_changes
  | Unpushed_changesets
  | Unsatisfied_invariant
  | Error of Error.t
[@@deriving compare, sexp_of]

type t = private one_reason list (* invariant: Not empty *)
[@@deriving compare, sexp_of]

include Invariant.S with type t := t
include Equal.S     with type t := t

val to_string_hum : t -> string

val to_ascii_table_column_text : t -> string

val create : one_reason list -> t option

val error : Error.t -> t

val add : t -> t -> t

module Stable : sig
  module Model : T with type t = t

  module V1 : sig
    include Stable_without_comparator with type t = Model.t
    val of_model : Model.t -> t
  end
end
