(** Obligations for all files in a repo. *)

open! Core
open! Async
open! Import

type t = private
  { obligations_repo   : [ `Fake | `Actual of Obligations_repo.t ]
  ; by_path            : (Path_in_repo.t, Review_attributes.t) Hashtbl.t
  }
[@@deriving sexp_of]

val sorted_files_alist : t -> (Path_in_repo.t * Review_attributes.t) list

val file_owner : t -> Path_in_repo.t -> User_name.t Or_error.t

val create
  :  (module Hg_dependency.S)
  -> ?skip_full_repo_checks   : unit
  -> repo_root                : Repo_root.t
  -> dirs                     : [ `All
                                | `Below of Path_in_repo.t
                                | `Only_this of Path_in_repo.t
                                ]
  -> manifest                 : Path_in_repo.t list
  -> aliases                  : User_name_by_alternate_name.t
  -> unit
  -> (t Or_error.t * Obligations_version.t Or_error.t) Deferred.t

val low_review_files : t -> Path_in_repo.Set.t Build_projection_name.Map.t

val fake
  :  manifest : Path_in_repo.t list
  -> Review_attributes.t
  -> t

module Stable : sig
  module V5 : sig
    type model
    type t =
      { obligations_repo : Obligations_repo.Stable.V5.t
      ; by_path          : (Path_in_repo.Stable.V1.t * Review_attributes.Stable.V2.t) list
      }
    include Stable_without_comparator with type t := t
    val to_model : t -> model
    val of_model : model -> t
  end with type model := t
end
