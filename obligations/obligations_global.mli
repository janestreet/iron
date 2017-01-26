(** Global spec file [.fe/obligations-global.sexp]. *)

open! Core
open! Import

module Declaration : sig
  type t [@@deriving sexp_of]

  val of_annotated_sexp : Sexp.Annotated.t -> t
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

val eval
  : Declaration.t list
  -> obligations_global : Path.t
  -> aliases : User_name_by_alternate_name.t
  -> t Or_error.t
