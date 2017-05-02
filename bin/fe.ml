open! Core
open Iron_common.Std
open Iron.Std

let () = Int_conversions.sexp_of_int_style := `Underscores

let () = print_elapsed [%here]

let () = Async.Writer.behave_nicely_in_pipeline ()

let server_state_invariant =
  Command.group ~summary:"check server-state invariant"
    [ "check-backup-in"         , Iron_server.Cmd_server.load_state_and_check_invariants
    ; "check-most-recent-backup", Deploy.check_invariants_of_most_recent_prod_backup
    ; "check-running-server"    , Iron_client.Cmd_check_invariant.command
    ]
;;

let invariant =
  Command.group ~summary:"check invariant"
    [ "check-workspaces"  , Iron_client.Cmd_workspace.check_workspaces_invariant_command
    ; "included-features" , Iron_client.Cmd_internal_invariant_included_features.command
    ; "server-state"      , server_state_invariant
    ]
;;

let internal =
  Command.group ~summary: "various commands for Iron developers"
    [ "archived-features-cache"      , Iron_client.Cmd_archived_features_cache_internal.command
    ; "cached-attributes"            , Iron_client.Cmd_cache_internal.command
    ; "catch-up"                     , Iron_client.Cmd_catch_up_internal.command
    ; "clear-bookmarks-without-feature" , Iron_client.Cmd_clear_bookmarks_without_feature.command
    ; "command-rpc"                  , Iron_client.Cmd_internal_command_rpc.command
    ; "de-alias"                     , Iron_client.Cmd_de_alias_feature.command
    ; "diffs"                        , Iron_hg.Diff4s_for_diamond.command
    ; "dump"                         , Iron_client.Cmd_dump.command
    ; "dynamic-upgrade"              , Iron_client.Cmd_dynamic_upgrade_internal.command
    ; "event-subscriptions"          , Iron_client.Cmd_event_subscriptions_internal.command
    ; "fully-reviewed-edge"          , Iron_client.Cmd_fully_reviewed_edge.internal
    ; "fully-reviewed-revision"      , Iron_client.Cmd_fully_reviewed_revision.internal
    ; "hg-path"                      , Iron_hg.Hg.hg_path_command
    ; "hydra-worker"                 , Iron_client.Hydra_worker.command
    ; "infer-base"                   , Iron_client.Hydra_worker.infer_base_command
    ; "invariant"                    , invariant
    ; "mark-fully-reviewed"          , Iron_client.Cmd_has_moved.command
                                         ~moved_to:[ "fe"; "tools"; "mark-fully-reviewed"; ]
    ; "need-diff4s-starting-from"    , Iron_client.Cmd_need_diff4s_starting_from.command
    ; "notify-on-descendant-updates" , Iron_client.Cmd_notify_on_descendant_updates.command
    ; "notify-on-feature-updates"    , Iron_client.Cmd_notify_on_feature_updates.command
    ; "push-events"                  , Iron_client.Cmd_push_events.command
    ; "remove-color"                 , Iron_client.Cmd_remove_color.command
    ; "render-archive-email"         , Iron_client.Cmd_show.render_archive_email_command
    ; "render-release-email"         , Iron_client.Cmd_show.render_release_email_command
    ; "rpc-to-server"                , Iron_client.Cmd_internal_rpc_to_server.command
    ; "scaffold"                     , Iron_client.Cmd_scaffold_internal.command
    ; "serverless-hydra-worker"      , Iron_client.Hydra_worker.serverless_command
    ; "session"                      , Iron_client.Cmd_session_internal.command
    ; "set-brains-to-goal-if-edge"   , Iron_client.Cmd_set_brains_to_goal_if_edge.command
    ; "show-lines-required-to-separate-ddiff-hunks"
    , Iron_client.Cmd_show.show_lines_required_to_separate_ddiff_hunks
    ; "show-repo-for-hg-operations"  , Iron_client.Cmd_workspace.repo_for_hg_operations_command
    ; "terminal-width"               , Iron.Param.terminal_width_command
    ; "timed-events"                 , Iron_client.Cmd_timed_events_internal.command
    ; "unclean-workspaces"           , Iron_client.Cmd_unclean_workspaces_internal.command
    ; "worker-cache"                 , Iron_client.Cmd_worker_cache_internal.command
    ; Iron_client.Cmd_process_num_lines_in_diff4.command
    ; Fe.show_supported_iron_rpcs
    ]
;;

let tools =
  Command.group ~summary: "miscellaneous commands"
    [ "build-order-sort"             , Iron_client.Cmd_build_order_sort.command
    ; "complete-feature-path"        , Iron_client.Cmd_complete_feature_path.command
    ; "feature-exists"               , Iron_client.Cmd_feature_exists.command
    ; "feature-table-of-csv"         , Iron_client.Cmd_feature_table_of_csv.command
    ; "force-retry"                  , Iron_client.Cmd_force_retry.command
    ; "fully-reviewed-edge"          , Iron_client.Cmd_fully_reviewed_edge.tools
    ; "fully-reviewed-revision"      , Iron_client.Cmd_fully_reviewed_revision.tools
    ; "get-feature-email-recipients" , Iron_client.Cmd_get_feature_email_recipients.command
    ; "hg"                           , Iron_hg.Hg.hg_command
    ; "hg-hooks"                     , Iron_client.Cmd_hg_hooks.command
    ; "pairwise-common-revisions"    , Iron_client.Cmd_pairwise_common_revisions.command
    ; "ping"                         , Iron_client.Cmd_ping.command
    ; "restore-bookmark"             , Iron_client.Cmd_restore_bookmark.command
    ; "review-ddiff"                 , Iron_client.Cmd_review_ddiff.command
    ; "strip-crs"                    , Iron_client.Cmd_strip_crs.command
    ; "mark-fully-reviewed"          , Iron_client.Cmd_mark_fully_reviewed.command
    ; "unbookmarked-head"            , Iron_client.Cmd_unbookmarked_head.command
    ; "validate-ferc"                , Iron.Client_config.validate_config
    ; "wait-for-hydra"               , Iron_client.Cmd_wait_for_hydra.command
    ]
;;

let workspace_command =
  Command.group
    ~summary:"managing clones and shares for features"
    ~readme:Iron.Client_config.Workspaces.readme
    ([ "update-clean-workspaces", Iron_client.Cmd_update_clean_workspaces.command
     ]
     @ Iron_client.Cmd_workspace.workspace_commands)
;;

let cmd_top =
  Command.group ~summary: "manage feature-based code development"
    [ "admin"          , Admin.command
    ; "archive"        , Iron_client.Cmd_archive.command
    ; "brain"          , Iron_client.Cmd_brain.command
    ; "catch-up"       , Iron_client.Cmd_catch_up.command
    ; "change"         , Iron_client.Cmd_change.command
    ; "compress"       , Iron_client.Cmd_compress.command
    ; "conflicts"      , Iron_client.Cmd_conflicts.command
    ; "copy"           , Iron_client.Cmd_copy.command
    ; "create"         , Iron_client.Cmd_create.command
    ; "crs"            , Iron_client.Cmd_crs.command
    ; "delete"         , Iron_client.Cmd_delete.command
    ; "description"    , Iron_client.Cmd_description.command
    ; "diff"           , Iron_client.Cmd_diff.command
    ; "disable-review" , Iron_client.Cmd_enable_review.disable_command
    ; "enable-review"  , Iron_client.Cmd_enable_review.enable_command
    ; "fact"           , Iron_client.Cmd_fact.command
    ; "glog"           , Iron_client.Cmd_glog.command
    ; "internal"       , internal
    ; "is-releasable"  , Iron_client.Cmd_is_releasable.command
    ; "is-unlocked"    , Iron_client.Cmd_is_unlocked.command
    ; "list"           , Iron_client.Cmd_list.command
    ; "lock"           , Iron_client.Cmd_lock.command
    ; "log"            , Iron_client.Cmd_log.command
    ; "obligations"    , Iron_client.Cmd_obligations.command
    ; "projects"       , Iron_client.Cmd_projects.command
    ; "rebase"         , Iron_client.Cmd_rebase.command
    ; "release"        , Iron_client.Cmd_release.command
    ; "remind"         , Iron_client.Cmd_remind.command
    ; "rename"         , Iron_client.Cmd_rename.command
    ; "review"         , Iron_client.Cmd_review.command
    ; "second"         , Iron_client.Cmd_second.command
    ; "session"        , Iron_client.Cmd_session.command
    ; "show"           , Iron_client.Cmd_show.command
    ; "todo"           , Iron_client.Cmd_todo.command
    ; "tools"          , tools
    ; "unarchive"      , Iron_client.Cmd_unarchive.command
    ; "unlock"         , Iron_client.Cmd_unlock.command
    ; "unsecond"       , Iron_client.Cmd_unsecond.command
    ; "update"         , Iron_client.Cmd_update.command
    ; "widen-reviewing", Iron_client.Cmd_widen_reviewing.command
    ; "workspace"      , workspace_command
    ]
;;

let () = Command.run cmd_top
