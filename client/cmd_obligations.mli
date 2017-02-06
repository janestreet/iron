open! Core
open! Async
open! Import

module List_groups : sig
  val main : Implement_command_rpc (Fe.Obligations.List_groups).t
end

module List_users : sig
  val main : Implement_command_rpc (Fe.Obligations.List_users).t
end

val create_obligations
  :  ?skip_full_repo_checks:unit
  -> ?resolve_aliases:bool
  -> Repo_root.t
  -> dirs:[ `All
          | `Below     of Path_in_repo.t
          | `Only_this of Path_in_repo.t
          ]
  -> Obligations.t Deferred.t

val command : Command.t
