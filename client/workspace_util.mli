open! Core
open! Async
open! Import

(** Memoize the query [List_root_features].  Root features don't change often *)
val root_features_exn_with_memo
  : unit -> List_root_features.Reaction.one Feature_name.Map.t Deferred.t

val find_remote_repo_path_exn
  : Feature_path.t -> Remote_repo_path.t Deferred.t

val create_repo_if_it_does_not_exist
  :  [ `Clone_of of Feature_name.t
     | `Share_of of Feature_path.t
     ]
  -> repo_root_abspath : Abspath.t
  -> create_repo       : (unit -> unit Deferred.t)
  -> Repo_root.t Deferred.t
