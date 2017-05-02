open! Core
open! Import

val print_table
  :  Ascii_table.t
  -> display_ascii      : bool
  -> max_output_columns : int
  -> unit

val attribute_table
  :  display_ascii                    : bool
  -> max_output_columns               : int
  -> show_feature_id                  : bool
  -> show_lock_reasons                : bool
  -> show_inheritable_attributes      : bool
  -> show_next_steps                  : bool
  -> show_full_compilation_status     : bool
  -> Feature.t
  -> string

val attribute_table_with_fields
  :  display_ascii                                : bool
  -> max_output_columns                           : int
  (** [~next_steps:(Some [])] will cause [Report_iron_bug] to be displayed. *)
  -> next_steps                                   : Next_step.t list option
  -> ?compilation_status_to_display               : Compilation_status_to_display.t
  -> ?feature_id                                  : Feature_id.t
  -> ?whole_feature_followers                     : User_name.Set.t
  -> ?whole_feature_reviewers                     : User_name.Set.t
  -> ?owners                                      : User_name.t list
  -> ?base                                        : Rev.t
  -> ?base_facts                                  : Rev_facts.t Or_pending.t
  -> ?next_base_update                            : Next_base_update.t
  -> ?crs_are_enabled                             : bool
  -> ?crs_shown_in_todo_only_for_users_reviewing  : bool
  -> ?xcrs_shown_in_todo_only_for_users_reviewing : bool
  -> ?next_bookmark_update                        : Next_bookmark_update.t
  -> ?has_bookmark                                : bool
  -> ?tip                                         : Rev.t
  -> ?tip_facts                                   : Rev_facts.t Or_pending.t
  -> ?base_is_ancestor_of_tip                     : Rev_facts.Is_ancestor.t Or_pending.t
  -> ?is_permanent                                : bool
  -> ?is_archived                                 : Is_archived.t
  -> ?seconder                                    : User_name.t option
  -> ?review_is_enabled                           : bool
  -> ?reviewing                                   : Reviewing.t
  -> ?properties                                  : Properties.t
  -> ?has_children                                : bool
  -> ?release_process                             : Release_process.t
  -> ?who_can_release_into_me                     : Who_can_release_into_me.t
  -> ?send_email_to                               : Email_address.Set.t
  -> ?send_email_upon                             : Send_email_upon.Set.t
  -> ?locked : (Lock_name.t * Feature.Locked.t list) list
  -> ?inheritable_attributes                      : Inheritable_attributes.t
  -> ?show_lock_reasons                           : bool
  -> ?show_inheritable_attributes                 : bool
  -> ?show_is_archived_if_not_archived            : bool
  -> unit
  -> string

val review_sessions_in_progress_table : User_name.Set.t -> Ascii_table.t
val unclean_workspaces_table : Unclean_workspace_reason.t User_name.Map.t -> Ascii_table.t

val header_and_description : Feature_path.t -> description:string -> string

val command : Command.t

module Event : sig
  type t =
    | Released
    | Archived of { reason_for_archiving : string }
end

val render_email_body
  :  Feature.t
  -> included_features_order : Feature.Sorted_by.t
  -> event:Event.t
  -> string

val render_release_email_command : Command.t
val render_archive_email_command : Command.t

val show_lines_required_to_separate_ddiff_hunks : Command.t
