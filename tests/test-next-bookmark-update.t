  $ start_test

Create a feature

  $ setup_repo_and_root a
  $ fe show root -next-bookmark-update | matches Update_expected_since

When we receive an update-bookmark, we're good:

  $ feature_to_server root
  $ fe show root -next-bookmark-update
  No_update_expected

When hydra tells fe about a different revision, it becomes pending:

  $ tip=$(fe show -tip); simple-sync-state root ${tip:0:12}
  ((bookmarks_to_rerun ()))
  $ fe show root -next-bookmark-update
  No_update_expected
  $ simple-sync-state root 0123456789ab
  ((bookmarks_to_rerun (root)))
  $ fe show root -next-bookmark-update | matches Update_expected_since

This hydra state is lost on restart, so we think nothing is incoming:

  $ fe-server stop
  $ fe-server start
  $ fe show root -next-bookmark-update
  No_update_expected

After two retries, we still expect a bookmark update.

  $ simple-sync-state root 0123456789ab
  ((bookmarks_to_rerun (root)))
  $ simple-sync-state root 0123456789ab
  ((bookmarks_to_rerun (root)))
  $ fe show root -next-bookmark-update | matches Update_expected_since

But when hydra tells us it's idle, we neither request nor expect a bookmark update.

  $ simple-sync-state root 0123456789ab
  ((bookmarks_to_rerun ()))
  $ fe show root -next-bookmark-update
  (No_update_expected_due_to_iron_bug
   "Iron was unable to process this feature")

Now if the worker blows up while trying to feed the server

  $ touch make-the-status-unclean
  $ feature_to_server root

The server should know about the failure:

  $ fe show root -next-bookmark-update \
  >      | matches '(No_update_expected_due_to_iron_bug.*"hg repository is not clean"'

And [fe show] should report the bug.

  $ fe show root -next-step
  (Report_iron_bug)

After a restart, fe thinks we're fine until the first synchronize-state, at which point
the error pops back:

  $ fe-server stop
  $ fe-server start
  $ fe show root -next-bookmark-update
  No_update_expected
  $ simple-sync-state root 0123456789ab
  ((bookmarks_to_rerun ()))
  $ fe show root -next-bookmark-update \
  >     | matches '(No_update_expected_due_to_iron_bug.*"hg repository is not clean"'

but not if hydra says it's working on it:

  $ simple-sync-state root 0123456789ab Pending_or_working_on_it
  ((bookmarks_to_rerun ()))
  $ fe show root -next-bookmark-update | matches Update_expected_since

but is cleared by a correct update (until the next synchronize-state, at least):

  $ rm make-the-status-unclean
  $ feature_to_server root
  $ simple-sync-state root ${tip:0:12}
  ((bookmarks_to_rerun ()))
  $ fe show root -next-bookmark-update
  No_update_expected
