(** A repo clone is a directory on disk that holds the hg clone of the repo for a root
    feature, which is used for all shares of feature in that tree.

    The directory is: ${WORKSPACE_BASEDIR}/${ROOT_FEATURE}/+clone+

    A clone has "spare shares", which are shares on disk not currently associated with a
    feature, and that are used to quickly create a share.
*)

open! Core
open! Async
open! Import

val clone_file_name : File_name.t (** ["+clone+"] *)

type t [@@deriving sexp_of]

(** Accessors *)

val root_feature     : t -> Feature_name.t
val remote_repo_path : t -> Remote_repo_path.t
val repo_root        : t -> Repo_root.t
val tip_on_server    : t -> Rev.t

(** Create *)

val force : root_feature:Feature_name.t -> t Deferred.t
val find  : root_feature:Feature_name.t -> t option Deferred.t
val list  : unit -> t list Deferred.t

(** Side effect *)

val pull_all_revs : t -> unit Deferred.t

(** Shares *)

(** [refresh_spare_shares] creates spare shares so that there are [desired_num_spares],
    and updates every share to [update_to]. *)
val refresh_spare_shares
  :  t
  -> desired_num_spares : int
  -> update_to          : Rev.t
  -> unit Deferred.t

(** [create_share] uses a spare share if there is one. *)
val create_share_exn
  :  t
  -> dst_repo_root_abspath : Abspath.t
  -> Repo_root.t Deferred.t

val workspaces_basedir : unit -> Abspath.t
