open! Core
open! Async
open! Import

val command : Command.t
val catch_up_review_command : Command.t

val print_catch_up_header_and_description
  :  Get_catch_up_session.Catch_up_session.t
  -> unit

val print_catch_up_attribute_table
  :  Get_catch_up_session.Catch_up_session.t
  -> display_ascii             : bool
  -> max_output_columns        : int
  -> unit

module Account_for_reviewer_warnings : sig
  type t =
    { reviewer_in_feature : Reviewer.t
    ; line_count_to_finish_session : Line_count.Review.t
    ; line_count_to_goal : Line_count.Review.t Or_error.t Or_pending.t
                             Get_review_session.Line_count_to_goal.t
    }
end

val print_introduction_summary_for_review
  :  feature_path        : Feature_path.t
  -> review_session_tip  : Rev.t
  -> reviewer_in_session : Reviewer.t
  -> warn_reviewer       : Account_for_reviewer_warnings.t option
  -> diff4s_to_review    : Diff4_to_review.t list
  -> display_ascii       : bool
  -> max_output_columns  : int
  -> unit

val print_diff4s
  :  repo_root:Repo_root.t
  -> diff4s:Diff4.t list
  -> reviewer:[ `Reviewer of Reviewer.t | `Whole_diff_plus_ignored ]
  -> context:int
  -> lines_required_to_separate_ddiff_hunks:int
  -> unit Deferred.t

(** If [pull_and_update] runs without raising, it is supposed to guarantee that:

    A. the active bookmark is [feature_path], and
    B. the review-session tip is an ancestor of [feature_path]

    It also encourages, but does not require:

    C. the server's feature tip is an ancestor of [feature_path]

    However, it may be the case that [A && B && not C] and the repo is unclean (and hence
    it cannot update), in which case we are willing to live with just [A && B].
*)
val pull_and_update
  :  repo_root          : Repo_root.t
  -> remote_repo_path   : Remote_repo_path.t
  -> feature_path       : Feature_path.t
  -> review_session_tip : Rev.t
  -> feature_tip        : Rev.t
  -> unit Deferred.t

val may_modify_others_review_exn
  :  Feature_path.t
  -> reason       : [ `Not_supported | `This of string ]
  -> whose_review : User_name.Or_all_or_all_but.t
  -> unit Deferred.t

val confirm_review_session_id_exn
  :  Repo_root.t Or_error.t
  -> action             : string
  -> feature_path       : Feature_path.t
  -> for_               : User_name.t
  -> which_session      : Which_session.t
  -> display_ascii      : bool
  -> max_output_columns : int
  -> [ `Id of Session_id.t | `Cancelled ] Deferred.t

val commit_session_exn
  :  Repo_root.t Or_error.t
  -> feature_path       : Feature_path.t
  -> for_               : User_name.t
  -> which_session      : Which_session.t
  -> display_ascii      : bool
  -> max_output_columns : int
  -> [ `Committed | `Cancelled ] Deferred.t

(** The list returned by [create_files_for_review] has the same length as [~diff4s], and
    each element gives the hunks of the corresponding [diff4] in [diff4s]. *)
val create_files_for_review
  :  temp_dir:Abspath.t
  -> repo_root:Repo_root.t
  -> diff4s:Diff4.t list
  -> reviewer:[ `Reviewer of Reviewer.t
              | `Whole_diff_plus_ignored
              ]
  -> context:int
  -> lines_required_to_separate_ddiff_hunks:int
  -> Pdiff4.Hunk.t list Or_error.t Deferred.t list Deferred.t
