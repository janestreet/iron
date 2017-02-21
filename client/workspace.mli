(** A workspace is one or several hg shares dedicated to a feature.  Workspaces are placed
    in the user's workspace basedir, and they include all the feature repo's satellites if
    it is scaffolded.  Share basenames are [+share+]. *)

open! Core
open! Async
open! Import

type t [@@deriving sexp_of]

(** Accessors *)

val feature_path     : t -> Feature_path.t
val center_repo_root : t -> Repo_root.t
val feature_id       : t -> Feature_id.t option

(** Create *)

val find  : ?feature_id:Feature_id.t -> Feature_path.t -> t option Deferred.t
val force : Workspace_hgrc.Feature.t -> t Deferred.t

(** [list] returns features sorted by name. *)
val list  : unit -> t list Deferred.t

(** Side effect *)

val distclean  : t -> unit Deferred.t
val delete     : t -> unit Deferred.t
val move_to    : t -> Feature_id.t -> Feature_path.t -> unit Deferred.t
val kill_build : t -> unit Deferred.t

(** Checks *)

module Unclean_status : sig
  type t =
    | Clean
    | Unclean of Unclean_workspace_reason.t
end

val unclean_status : t -> Unclean_status.t Deferred.t

(** Path *)

val extract_feature_from_workspace_share_path : Repo_root.t -> Feature_path.t option
