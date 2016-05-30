open! Core.Std
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val deserializer : t Deserializer.t

val dump
  : t
  ->[ `All_users
    | `User of User_name.t
    ]
  -> Sexp.t

val find_user
  : t
  -> User_name.t
  -> Unclean_workspace.t list Machine.Map.t

val update
  : t
  -> _ Query.t
  -> Iron_protocol.Update_unclean_workspaces.Action.t
  -> unit

val remove_machine_exn
  : t
  -> _ Query.t
  -> User_name.t
  -> Machine.t
  -> unit

val remove_user_exn
  : t
  -> _ Query.t
  -> User_name.t
  -> unit

val users
  : t
  -> User_name.Set.t

val find_feature
  : t
  -> Feature_path.t
  -> Unclean_workspace_reason.t User_name.Map.t
