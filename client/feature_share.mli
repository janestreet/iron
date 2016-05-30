(** A feature share is an hg share of a feature in the user's workspace basedir, including
    all the feature repo's satellites if it is scaffolded.  Share basenames are [+share+].
*)

open! Core.Std
open! Async.Std
open! Import

type t [@@deriving sexp_of]

(** Accessors *)

val feature_path     : t -> Feature_path.t
val center_repo_root : t -> Repo_root.t

(** Create *)

val find  : Feature_path.t -> t option Deferred.t
val force : Feature_path.t -> t Deferred.t

(** [list] returns features sorted by name. *)
val list  : unit -> t list Deferred.t

(** Side effect *)

val distclean  : t -> unit Deferred.t
val delete     : t -> unit Deferred.t
val move_to    : t -> Feature_path.t -> unit Deferred.t
val kill_build : t -> unit Deferred.t

(** Checks *)

val check_workspace_invariant : t -> unit Or_error.t Deferred.t

module Unclean_status : sig
  type t =
    | Clean
    | Unclean of Unclean_workspace_reason.t
end

val unclean_status : t -> Unclean_status.t Deferred.t

(** Path *)

val extract_feature_from_workspace_share_path : Repo_root.t -> Feature_path.t option
