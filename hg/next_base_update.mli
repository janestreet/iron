(** What Iron server thinks for a feature about the next base update coming from a
    client. *)

open! Core
open! Import

module Update_expected : sig
  type t =
    { rev            : Rev.t
    ; by             : User_name.t
    ; expected_since : Time.t
    }
  [@@deriving sexp_of]

  include Invariant.S with type t := t
end

type t =
  | No_update_expected
  | Update_expected of Update_expected.t
[@@deriving sexp_of]

include Invariant.S with type t := t

module Stable : sig
  module V1 : Stable_without_comparator with type t = t
end
