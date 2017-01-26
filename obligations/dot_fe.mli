(** A per-directory Iron obligations file, [.fe.sexp]. *)

open! Core
open! Import

module Declaration : sig
  type t [@@deriving sexp_of]

  val of_annotated_sexp : Sexp.Annotated.t -> t
end

type t = Declaration.t list

val eval
  :  t
  -> dot_fe                                      : Path.t     (** for error messages *)
  -> used_in_subdirectory                        : Relpath.t  (** for error messages *)
  -> used_in_subdirectory_declaration_is_allowed : bool
  -> files_in_directory                          : File_name.Set.t
  -> obligations_repo                            : Obligations_repo.t
  -> aliases                                     : User_name_by_alternate_name.t
  -> Review_attributes.t File_name.Map.t Or_error.t

val has_used_in_subdirectory : t -> bool
