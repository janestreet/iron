open! Core
open! Import

type ssh = private { host : string; path : Abspath.t }
type t = private
  | Ssh of ssh
  | File of Abspath.t
[@@deriving compare, sexp]

include Equal.S      with type t := t
include Hashable.S   with type t := t
include Stringable.S with type t := t
include Invariant.S  with type t := t

val is_in_jane : t -> bool

val jane_submissions : t

val null : t

(** Only works for repositories on the hg box. *)
val family : t -> string option

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
