(** Global spec file [.fe/obligations-global.sexp]. *)

open! Core.Std
open! Import

module Declaration : sig
  type t [@@deriving sexp]
end

(** A file's worth of declarations once evaluated *)
type t = private
  { disallow_useless_dot_fe : bool
  ; scrutinies              : Scrutiny.t Scrutiny_name.Map.t
  ; tags                    : Tag.Set.t
  ; users                   : Unresolved_name.Set.t
  ; groups                  : Groups.t
  ; obligations_version     : Obligations_version.t
  }
[@@deriving sexp_of]

val eval : Declaration.t list -> t Or_error.t

module Stable : sig
  module V3 : sig
    include Stable_without_comparator with type t = t
    val hash : t -> int
  end
end
