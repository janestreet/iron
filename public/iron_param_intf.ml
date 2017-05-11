open Core
open Import
open Iron

module type T = sig

  include module type of struct include Command.Param end

  val user_name    : User_name.t Arg_type.t
  val alias        : Alternate_name.t Arg_type.t
  val typo         : Alternate_name.t Arg_type.t
  val rev_arg_type : Raw_rev.t Arg_type.t
  val rev_from_string_40_or_local_repo_arg_type : Raw_rev.t Arg_type.t
  val time_span    : Time.Span.t Arg_type.t
  val time         : Time.t Arg_type.t

  val path_in_repo_arg_type : Path_in_repo.t Arg_type.t
  val path_in_repo          : Path_in_repo.t      t
  val paths_in_repo         : Path_in_repo.t list t

  module type Blangable = sig
    type t [@@deriving sexp]
    val to_string : t -> string
  end

  val blang_arg_type_with_completion
    : (module Blangable with type t = 'a)
    -> (unit -> 'a list Or_error.t)
    -> 'a Blang.t Arg_type.t

  module Which_diffs : sig
    type t =
      [ `All
      | `Reviewed
      | `Unreviewed
      | `Files of Path_in_repo.t list
      ]
    [@@deriving sexp_of]
  end

  module Which_files : sig
    type t =
      [ `All
      | `Files of Path_in_repo.t list
      ]
    [@@deriving sexp_of]

    val restrict_exn
      :  t
      -> 'a list
      -> path_in_repo : ('a -> Path_in_repo.t)
      -> from: string
      -> 'a list
  end

  module Review_sort : sig
    type t =
      [ `By_increasing_review_lines
      | `By_decreasing_review_lines
      | `Using_file of Abspath.t
      | `Build_order
      ]
    [@@deriving sexp_of]
  end

  val no_arg_flag : ?aliases:string list -> string -> doc:string -> bool t

  (** [resolved_file_path_arg_type] takes any file name -- including relative file names,
      such as ../lib/print.ml -- and resolves it into an absolute file path, using the
      process' current working directory *at the time of arg-parsing*. *)
  val resolved_file_path_arg_type : Abspath.t Arg_type.t

  val maybe_archived_feature : Maybe_archived_feature_spec.Command_line.t Or_error.t t

  val resolve_maybe_archived_feature_spec_exn
    : Maybe_archived_feature_spec.Command_line.t
    -> Maybe_archived_feature_spec.t Async.Deferred.t

  val params : 'a t list -> 'a list t

  val enum_anon      : string -> 'a Enum.t -> 'a t
  val enum_anon_list : string -> 'a Enum.t -> 'a list  t

  val enum_list     : string -> doc:string -> 'a Enum.t -> 'a list   t
  val enum_no_args
    :  ?aliases:('a -> string list)
    -> 'a Enum.t -> doc:(name:string -> 'a -> string) -> 'a list t
  val enum_optional : string -> doc:string -> 'a Enum.t -> 'a option t
  val enum_required : string -> doc:string -> 'a Enum.t -> 'a        t

  val bool_with : string -> default:bool -> doc:string -> bool t

  val email_addresses_option : switch:string -> Email_address.Set.t option t
  val user_list_option       : switch:string -> User_name.t list    option t
  val users_option           : switch:string -> User_name.Set.t     option t

  val properties_list_option : switch:string -> verb:string -> Property.t list option t
  val properties_set_option  : switch:string -> verb:string -> Property.Set.t option  t

  val inheritable_properties_set_option : switch:string -> Property.Set.t option t

  val property_values_flag : switch:string -> doc:string -> Properties.t option t

  val for_or_all_default_me  : User_name.Or_all.t t
  val for_or_all_default_all : User_name.Or_all.t t
  val for_or_all_required    : User_name.Or_all.t t

  val for_or_all_or_all_but_default_me : User_name.Or_all_or_all_but.t Or_error.t t

  (** Error out if [catch_up_for_me] is used in combination with [-for] for other users
      only. *)
  val create_catch_up_for_me
    : (is_reviewing_for:User_name.Or_all_or_all_but.t -> bool Or_error.t) t

  val archived_feature_path            : Feature_path.t Or_error.t      t
  val anon_feature_paths               : Feature_path.t list Or_error.t t
  val base                             : Raw_rev.t option             t
  val catch_up_feature_path                     : Feature_path.t Or_error.t t
  val catch_up_feature_path_or_current_bookmark : Feature_path.t Or_error.t t
  val active_or_catch_up_feature_path_or_current_bookmark : Feature_path.t Or_error.t t
  val context                          : ?default:int -> unit -> int      t
  val lines_required_to_separate_ddiff_hunks_override : int option t
  val lines_required_to_separate_ddiff_hunks_with_default : int t
  val dash_dash_rest                   : doc:string -> string list  t
  val description                      : string option              t

  val display_ascii                             : bool                             t
  val max_output_columns                        : int                              t

  (** default is [true] *)
  val may_modify_local_repo                     : bool                             t
  val no_bookmark                               : bool                             t

  (** default is [false].  For commands that do not enforce it by default. *)
  val update_local_repo                         : bool                             t

  val emacs                                     : bool                             t

  val even_though_empty                         : bool                             t
  val even_though_owner                         : bool                             t

  val even_if_locked                            : unit                             t

  val even_if_some_files_are_already_reviewed   : bool                             t

  val feature_arg_type
    : match_existing_feature:bool -> Feature_path.t Or_error.t Arg_type.t
  val feature_path                              : Feature_path.t Or_error.t        t
  val absolute_feature_path                     : Feature_path.t Or_error.t t
  val absolute_feature_path_option              : Feature_path.t option Or_error.t t
  val feature_id                                : Feature_id.t                     t
  val feature_id_option                         : Feature_id.t option              t
  val feature_id_list                           : Feature_id.t list                t
  val feature_path_option                       : Feature_path.t option Or_error.t t
  val feature_path_or_current_bookmark          : Feature_path.t Or_error.t        t
  val unverified_workspace_arg_type             : Feature_path.t Or_error.t Arg_type.t
  val clone_of_root_feature_of : doc:string -> Feature_name.Set.t Or_error.t t

  val current_bookmark : unit -> Feature_path.t Or_error.t Async.Deferred.t

  val feature_path_flagged
    :  label : string
    -> doc   : string
    -> Feature_path.t Or_error.t t

  val feature_path_flagged_listed
    :  label : string
    -> doc   : string
    -> Feature_path.t list Or_error.t t

  val root_feature                     : Feature_name.t             t

  (** [for_]'s default is [User_name.unix_login]. *)
  val for_                             : User_name.t                t

  val include_active_cr_soons          : bool                       t
  val do_not_show_cr_soons             : bool                       t
  val do_not_show_unclean_workspaces   : bool                       t
  val which_session                    : Which_session.t            t
  val session_id_required              : Session_id.t               t
  val review_reason                    : string                     t
  val reason_for_archiving             : string                     t
  val diff4_in_session_ids             : Diff4_in_session.Id.t list t
  val lock_names                       : Lock_name.t list           t
  val lock_reason                      : string                     t
  val depth_option                     : int option                 t
  val metric_values                    : float list                 t
  val metric_name                      : Metric_name.t              t
  val metric_name_option               : Metric_name.t option       t
  val metric_name_regex_list_option
    : doc:string
    -> Regex.t list Or_error.t option t

  (** [interactive] sets [Interactive.interactive]. *)
  val interactive                      : unit                       t

  val may_repartition_crs              : bool                       t
  val new_base                         : Raw_rev.t option           t
  val owners                           : User_name.t list           t
  val permanent                        : bool                       t
  val projections                      : Build_projection_name.t list t
  val remote_repo_path                 : Remote_repo_path.t option  t
  val resolved_file_path               : Abspath.t                  t
  val rev                              : Raw_rev.t                  t
  val send_email_upon                  : (switch:string
                                          -> doc:string
                                          -> Send_email_upon.Set.t option t)
  val set_base                         : Raw_rev.t option           t
  val set_description                  : string  option             t
  val set_is_permanent                 : bool    option             t
  val sort_build_order                 : bool                       t
  val tip                              : Raw_rev.t option           t
  val verbose                          : bool                       t
  val which_diffs                      : Which_diffs.t t
  val which_features
    : ?allow_rec_flag:bool (** default true *)
    -> ?allow_unexisting_features:bool (** default to false *)
    -> allow_empty_selection:bool
    -> default_to_current_bookmark:bool
    -> unit
    -> Which_features.t Async.Deferred.t Lazy.t t
  val which_files                      : Which_files.t t
  val maybe_sort_review_files          : Review_sort.t option t

  val without_enough_whole_feature_reviewers : bool t
  val included_features_order : Iron.Feature.Sorted_by.t Or_error.t t

  val terminal_width_command : Command.t
end

module type Iron_param = sig
  module T : T
  include module type of struct include T end
  module Let_syntax : sig
    module Let_syntax : sig
      val return : 'a -> 'a t
      val map    : 'a t -> f:('a -> 'b) -> 'b t
      val both   : 'a t -> 'b t -> ('a * 'b) t
      module Open_on_rhs : module type of struct include T end
    end
  end
end
