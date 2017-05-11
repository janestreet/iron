open! Core
open! Import
open Iron

type t

module Cmd : sig
  module Create : sig
    val reviewing : t -> [ `Whole_feature_reviewers
                         | `First_owner
                         ] option
  end

  module List : sig
    val depth : t -> int option
  end

  module Obligations_show : sig
    val display_in_table : t -> bool
  end

  module Rebase : sig
    val merge_tool : t -> Merge_tool.t option
  end

  module Review : sig
    val do_not_modify_local_repo : t -> bool
    val emacs                    : t -> bool
    val sort_build_order         : t -> bool
  end

  module Show : sig
    val omit_completed_review         : t -> bool
    val omit_unclean_workspaces_table : t -> bool
    val show_compilation_status       : t -> bool
    val show_feature_id               : t -> bool
    val show_full_compilation_status  : t -> bool
    val show_inheritable_attributes   : t -> bool
    val show_lock_reasons             : t -> bool
  end

  module Todo : sig
    val do_not_show_cr_soons           : t -> bool
    val do_not_show_unclean_workspaces : t -> bool
  end

  module Wait_for_hydra : sig
    val update_local_repo : t -> bool
  end

  (** Common to multiple commands *)
  val context : t -> int option
end

module Workspaces : sig
  val readme           : unit -> string
  val are_enabled      : t -> bool

  val unclean_workspaces_detection_is_enabled : t -> bool
  val unclean_workspaces_detection_max_concurrent_jobs : t -> int

  val auto_update_clean_workspaces_is_enabled : t -> bool

  val unclean_workspaces_detection_includes_shelved_changes : t -> bool

  (** The rest of the accessors raise if workspaces are not enabled. *)

  val are_enabled_exn    : t -> unit
  val basedir            : t -> Abspath.t
  val do_not_distclean   : t -> Feature_path.Set.t
  val do_not_auto_update : t -> Feature_path.Set.t
end

(** [directory_order] is used to provide additional graph edges between directories for
    the topological sort on directories done by [-sort-build-order].  One can have
    multiple [directory_order] stanzas, each adding a list of directories.  For each [d1;
    d2; d3 ...] in [directory_order], edges are added: [d1 --> d2], [d2 --> d3], ...

    This is a useful hack until jenga outputs better dependency information.  E.g.:

    {[
      (directory_order ("app/fe/bin" "app/fe/tests"))
    ]}

    Additional edges may cause cycles in the dependency graph and cause the topological
    sort to fail. *)
val directory_order : t -> Path_in_repo.t list list

val may_infer_feature_path_from_current_bookmark : t -> bool

val pager_for_review : t -> string option

val send_push_events_to_server : t -> bool

val show_commit_session_warning : t -> bool

include Iron_common.Std.Make_client_config.S with type t := t
