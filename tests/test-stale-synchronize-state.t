Just after receiving a bookmark update, the next step of the feature is not wait-for-hydra
anymore:

  $ start_test
  $ setup_repo_and_root a
  $ feature_to_server root -fake-valid-obligations
  $ echo b > a
  $ hg commit -m b
  $ new_tip=$(hg log -r . --template "{node|short}")
  $ simple-sync-state root $new_tip Pending_or_working_on_it
  ((bookmarks_to_rerun ()))
  $ feature_to_server root -fake-valid-obligations
  $ fe show -next-steps root
  (Enable_review)

We ignore synchronize-state rpcs that look stale, to avoid making the feature spuriously
pending again:

  $ simple-sync-state root $new_tip Pending_or_working_on_it
  ((bookmarks_to_rerun ()))
  $ fe show -next-steps root
  (Enable_review)
  $ simple-sync-state root $new_tip Pending_or_working_on_it
  ((bookmarks_to_rerun ()))
  $ fe show -next-steps root
  (Enable_review)

But if we get too many of them, then we accept them because it's looking like hydra is
actually computing something:

  $ simple-sync-state root $new_tip Pending_or_working_on_it
  ((bookmarks_to_rerun ()))
  $ fe show -next-steps root
  (Wait_for_hydra)
