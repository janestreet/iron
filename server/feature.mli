open! Core
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val deserializer
  : dynamic_upgrade_state:Dynamic_upgrade.State.t
  -> t Deserializer.t

(** Accessors. *)
val base                      : t -> Rev.t
val next_base_update          : t -> Next_base_update.t
val base_is_ancestor_of_tip   : t -> Rev_facts.Is_ancestor.t Or_pending.t
val base_facts                : t -> Rev_facts.t Or_pending.t
val cache_invalidator         : t -> Cached.Invalidator.t
val continuous_release_status : t -> Continuous_release_status.t
val crs_are_enabled           : t -> bool
val description               : t -> string
val diff_from_base_to_tip     : t -> Diff2s.t Or_error.t Or_pending.t
val has_bookmark              : t -> bool
val indexed_diff4s            : t -> Indexed_diff4s.t Or_error.t Or_pending.t
val inheritable_attributes    : t -> Feature_inheritable_attributes.t
val is_permanent              : t -> bool
val owners                    : t -> User_name.t list
val first_owner               : t -> User_name.t
val feature_id                : t -> Feature_id.t
val feature_path              : t -> Feature_path.t
val line_count_by_user_cached : t -> Line_count.Cached_in_feature.By_user.t Cached.t
val line_count_by_user        : t -> Line_count.Cached_in_feature.By_user.t Or_error.t
val lines_required_to_separate_ddiff_hunks : t -> int option
val locks                     : t -> Feature_locks.t
val next_bookmark_update      : t -> Next_bookmark_update.t
val next_steps_cached         : t -> Next_step.t list Cached.t
val next_steps                : t -> Next_step.t list
val properties                : t -> Properties.t
val find_property             : t -> Property.t -> Sexp.t option
val release_process           : t -> Release_process.t
val remote_repo_path          : t -> Remote_repo_path.t option
val rev_zero                  : t -> Rev.t
val review_analysis_cached    : t -> Review_analysis.t option Cached.t
val review_analysis           : t -> Review_analysis.t option
val review_is_enabled         : t -> bool
val num_lines                 : t -> int Or_error.t Or_pending.t
val seconder                  : t -> User_name.t option
val send_email_to             : t -> Email_address.Set.t
val send_email_upon           : t -> Send_email_upon.Set.t
val tip                       : t -> Rev.t
val tip_facts                 : t -> Rev_facts.t Or_pending.t
val who_can_release_into_me   : t -> Who_can_release_into_me.t
val whole_feature_followers   : t -> User_name.Set.t
val whole_feature_reviewers   : t -> User_name.Set.t

val allow_review_for : t -> (Allow_review_for.t, read) Ref.Permissioned.t

val crs_shown_in_todo_only_for_users_reviewing  : t -> bool
val xcrs_shown_in_todo_only_for_users_reviewing : t -> bool

val is_owner : t -> User_name.t -> bool

val all_whole_feature_reviewers_are_owners : t -> bool

val is_empty : t -> bool

val is_seconded : t -> bool

val tip_is_cr_clean : t -> bool

val users_with_review_session_in_progress : t -> User_name.Set.t Or_error.t

val user_is_currently_reviewing : t -> User_name.t -> bool

val to_protocol
  :  t
  -> is_archived        : Is_archived.t
  -> is_rebased         : bool
  -> remote_repo_path   : Remote_repo_path.t
  -> has_children       : bool
  -> cr_summary         : Cr_comment.Summary.t Or_error.t
  -> line_count_by_user : (User_name.t * Line_count.t) list Or_error.t
  -> users_with_unclean_workspaces : Unclean_workspace_reason.t User_name.Map.t
  -> Iron_protocol.Feature.t

(** The [serializer] argument is lazy because we don't want to create the serializer
    if [create_exn] raises. *)
val create_exn
  :  _ Query.t
  -> feature_id            : Feature_id.t
  -> feature_path          : Feature_path.t
  -> owners                : User_name.t list
  -> is_permanent          : bool
  -> description           : string
  -> base                  : Rev.t
  -> tip                   : Rev.t
  -> rev_zero              : Rev.t
  -> remote_repo_path      : Remote_repo_path.t option
  -> dynamic_upgrade_state : Dynamic_upgrade.State.t
  -> serializer            : Serializer.t Lazy.t
  -> t

val change
  :  t
  -> _ Query.t
  -> Iron_protocol.Change_feature.Update.t list
  -> Iron_protocol.Change_feature.Reaction.t

val clear_cached_attributes : t -> unit

val cr_soons : t -> Cr_soons.In_feature.t Or_error.t Or_pending.t

val force_hydra_retry : t -> unit

val invalidate_dependents : t -> unit

val rename_non_root : t -> _ Query.t -> to_:Feature_path.t -> unit

val review_goal : t -> Review_goal.t Or_error.t

val set_base : t -> _ Query.t -> Rev.t -> unit

val set_continuous_release_status
  : t
  -> Continuous_release_status.t
  -> unit

(** [expect_next_base_update_exn t ~for_ expected_base] is rejected if expected_base is
    equal to the current feature base. *)
val expect_next_base_update_exn
  : t
  -> for_:User_name.t
  -> Rev.t
  -> unit

val fire_next_base_update_expiration_if_applicable
  : t
  -> expiration_id:Timed_event.Id.t
  -> unit

(** [set_has_no_bookmark] does not erase currently known compilation status if any - this
    is a deliberate choice that allows fe to display previously known build info during
    situations of potential bookmark flickering (e.g. possibly [fe rename]). *)
val set_has_no_bookmark : t -> _ Query.t -> unit

(** [set_has_bookmark] allows one to enforce [has_bookmark = true] in call sites that
    don't have access to a compilation status (initialization of the server, right after
    [fe create], update-bookmark RPC, etc.). *)
val set_has_bookmark
  :  t
  -> _ Query.t
  -> compilation_status: [ `Update_with of Hydra_compilation_status.t
                         | `Keep_any_known_value
                         ]
  -> unit

val set_properties : t -> _ Query.t -> Properties.t -> unit

val set_review_is_enabled
  :  t
  -> _ Query.t
  -> bool
  -> unit

val set_crs_are_enabled
  :  t
  -> _ Query.t
  -> bool
  -> unit

val set_crs_shown_in_todo_only_for_users_reviewing
  :  t
  -> _ Query.t
  -> bool
  -> unit

val set_xcrs_shown_in_todo_only_for_users_reviewing
  :  t
  -> _ Query.t
  -> bool
  -> unit

val set_seconder
  :  t
  -> _ Query.t
  -> User_name.t option
  -> unit Or_error.t

val check_cached_attributes
  :  t
  -> ignore_diffs_in_errors:bool
  -> unit Or_error.t

val set_cached_attributes
  :  t
  -> line_count_by_user : Line_count.Cached_in_feature.By_user.t Cached.t
  -> next_steps         : Next_step.t list                       Cached.t
  -> review_analysis    : Review_analysis.t option               Cached.t
  -> unit

val set_latest_release
  :  t
  -> Latest_release.t
  -> unit

val synchronize_with_hydra
  :  t
  -> hydra_tip    : Node_hash.First_12.t
  -> hydra_status : [ `Done | `Pending_or_working_on_it ]
  -> [ `Retry | `Do_not_retry ]

(** Raises if either:
    - [tagged_tip]'s node hash isn't equal to the [tip t]'s node hash, or
    - feature is not seconded *)
val to_released_feature_exn
  :  t
  -> query      : _ Query.t
  -> tagged_tip : Rev.t option
  -> Released_feature.t

val include_released_feature
  :  t
  -> Released_feature.t
  -> unit

val clear_included_features
  :  t
  -> unit

val lock
  :  t
  -> query        : _ Query.t
  -> for_         : User_name.t
  -> lock_name    : Lock_name.t
  -> reason       : string
  -> is_permanent : bool
  -> unit Or_error.t

val unlock
  :  t
  -> query             : _ Query.t
  -> for_              : User_name.t
  -> lock_name         : Lock_name.t
  -> even_if_permanent : bool
  -> unit Or_error.t

val update_bookmark : t -> Iron_protocol.Update_bookmark.query -> unit Or_error.t
