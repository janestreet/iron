open! Core
open! Async

module Rpc_for (M : sig
    module Action   : T
    module Reaction : T
  end) : sig
  open M
  type t =
    ?connection:Connection.t
    -> Action.t
    -> Reaction.t Or_error.t Deferred.t
end

module Pipe_rpc_for (M : sig
    module Action   : T
    module Reaction : T
  end) : sig
  open M
  type t =
    ?connection:Connection.t
    -> Action.t
    -> Reaction.t Or_error.t Pipe.Reader.t Or_error.t Deferred.t
end

val archive                        : Rpc_for (Archive)                                  .t
val change_feature                 : Rpc_for (Iron_protocol.Change_feature)             .t
val compress                       : Rpc_for (Compress)                                 .t
val create                         : Rpc_for (Create)                                   .t
val enable_review                  : Rpc_for (Iron_protocol.Enable_review)              .t
val fact_action                    : Rpc_for (Iron_protocol.Fact_action)                .t
val fact_evidence                  : Rpc_for (Iron_protocol.Fact_evidence)              .t
val feature_exists                 : Rpc_for (Iron_protocol.Feature_exists)             .t
val feature_table_of_csv           : Rpc_for (Feature_table_of_csv)                     .t
val get_brain                      : Rpc_for (Iron_protocol.Get_brain)                  .t
val get_feature                    : Rpc_for (Iron_protocol.Get_feature)                .t
val get_feature_by_id              : Rpc_for (Iron_protocol.Get_feature.By_id)          .t
val get_feature_maybe_archived     : Rpc_for (Iron_protocol.Get_feature.Maybe_archived) .t
val list_feature_names             : Rpc_for (Iron_protocol.List_feature_names)         .t
val list_features                  : Rpc_for (Iron_protocol.List_features)              .t
val list_obligations_groups        : Rpc_for (Obligations.List_groups)                  .t
val list_obligations_users         : Rpc_for (Obligations.List_users)                   .t
val list_table                     : Rpc_for (Fe_list.Table)                            .t
val lock                           : Rpc_for (Lock)                                     .t
val notify_on_descendant_updates   : Pipe_rpc_for (Iron_protocol.Notify_on_descendant_updates).t
val notify_on_feature_updates      : Pipe_rpc_for (Iron_protocol.Notify_on_feature_updates)   .t
val ping                           : Rpc_for (Iron_protocol.Ping)                       .t
val rebase                         : Rpc_for (Rebase)                                   .t
val release                        : Rpc_for (Release)                                  .t
val rename                         : Rpc_for (Rename)                                   .t
val revision_is_fully_reviewed     : Rpc_for (Iron_protocol.Revision_is_fully_reviewed) .t
val second                         : Rpc_for (Iron_protocol.Second)                     .t
val supported_command_rpcs         : Rpc_for (Supported_command_rpcs)                   .t
val unarchive                      : Rpc_for (Unarchive)                                .t
val unlock                         : Rpc_for (Unlock)                                   .t
val update                         : Rpc_for (Update)                                   .t
val wait_for_hydra                 : Rpc_for (Wait_for_hydra)                           .t

val command_rpc_names : String.Set.t Lazy.t

(** Executables can export this command somewhere in their command to help keeping track
    of what version of the lib they have linked.  The string is a suggested name for the
    subcommand so that we have some consistency across binaries. *)
val show_supported_iron_rpcs : string * Command.t
