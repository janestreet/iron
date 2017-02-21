open! Core
open! Async
open! Import

module Unclean_workspace_row : sig
  type t
end

module Workspaces_status : sig
  type t = private
    { unclean_workspaces : Unclean_workspace.t list
    ; clean_workspaces   : Feature_path.t list
    }

  val compute_all : unit -> t Deferred.t
end

val unclean_workspaces_columns_and_rows
  : Unclean_workspace.t list
  -> Unclean_workspace_row.t Ascii_table.Column.t list * Unclean_workspace_row.t list

val compute_and_update_server_exn
  : for_ : User_name.t
  -> [ `All_features
     | `Features of Feature_path.t list
     ]
  -> unit Deferred.t

val command : Command.t
