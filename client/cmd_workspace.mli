open! Core
open! Async
open! Import

val workspaces_are_enabled : unit -> bool

(** [repo_for_hg_operations_exn feature ~use] returns a [repo_root] that can be used
    for hg operations dealing with [feature].

    If workspaces are not enabled, it returns [Repo_root.program_started_in], after
    checking that it's the right repo family.

    If workspaces are enabled, then [repo_for_hg_operations_exn] behaves according to
    [use]:

    - [`Clone] -- use the repo clone, creating it if it doesn't exist.

    - [`Share] -- use the feature share, creating it if it doesn't exist, unless
    the feature doesn't exist, in which case use the clone.

    - [`Share_or_clone_if_share_does_not_exist] -- use the feature share if it
    exists, or the repo clone if the feature share does not exist.

    So that people can use non-workspace repos even with workspaces enabled, if
    [Repo_root.program_started_in] is a repository that is not a workspace share and is in
    the same family as [feature], then [repo_for_hg_operations_exn] ignores [~use] and
    returns [Repo_root.program_started_in].

    [`Share] is only used in a few cases: [fe review], [fe update], and [fe
    wait-for-hydra], where it would likely be an error to be running them in the wrong
    feature share. [repo_for_hg_operations_exn ~use:`Share] fails if
    [Repo_root.program_started_in] is a feature share for the wrong feature.

    If a feature does not exist (for example when catching up on an archived feature),
    [`Share] and [`Share_or_clone_if_share_does_not_exist] behaves as if [`Clone] had been
    passed. *)
type use =
  [ `Clone
  | `Share
  | `Share_or_clone_if_share_does_not_exist
  ]

module Repo_root_kind : sig
  type t =
    | Clone
    | Program_started_in
    | Satellite
    | Workspace
  [@@deriving sexp_of]
end

val repo_for_hg_operations_exn
  :  Feature_path.t
  -> use : use
  -> Repo_root.t Deferred.t

val repo_for_hg_operations_and_kind_exn
  :  Feature_path.t
  -> use : use
  -> (Repo_root.t * Repo_root_kind.t) Deferred.t

val run_concurrent_actions_exn
  : get_feature_path: ('a -> Feature_path.t)
  -> action:string
  -> max_concurrent_jobs:int
  -> 'a list
  -> f:('a -> unit Deferred.t)
  -> unit Deferred.t

(** If workspaces are enabled, returns the clone of the common root of the features in the
    given list.  Returns [Repo_root.program_started_in] if workspaces are not enabled, or
    the list is empty, or the features don't have a common root. *)
val repo_for_hg_operations_use_clone_exn
  : Feature_path.t list -> Repo_root.t Deferred.t

(** Commands *)
val workspace_commands                 : (string * Command.t) list
val check_workspaces_invariant_command : Command.t
val repo_for_hg_operations_command     : Command.t

(** Perform the said operation only if workspaces are enabled, no op otherwise. *)
module If_enabled : sig

  (** Creates a workspace if it doesn't already exist. *)
  val create_workspace : Workspace_hgrc.Feature.t -> unit Deferred.t

  (** Deletes a workspace if it exists already, otherwise does nothing. *)
  val delete_workspace : Feature_path.t -> unit Deferred.t

  (** Move workspaces after a feature rename or compress.  If a workspace does not exist,
      does nothing. *)
  val rename_workspaces : Rename.t list -> unit Deferred.t

  val update_satellite_repos : center_repo_root:Repo_root.t -> unit Deferred.t
end
