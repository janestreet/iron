open! Core
open! Import

type t =
  | Current_session
  | This_session    of Session_id.t
[@@deriving sexp_of]

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
