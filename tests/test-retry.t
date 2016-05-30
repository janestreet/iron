Start test.

  $ start_test

Create a feature

  $ hg init repo
  $ cd repo
  $ echo a > a; hg add a; hg commit -m a
  $ remote_repo_path="$PWD"
  $ fe create a -description desc -remote-repo-path "$remote_repo_path"
  $ feature_to_server a

If hydra says there are no bookmarks, nothing to retry.

  $ fe internal rpc-to-server call synchronize-state <<EOF
  > ((remote_repo_path $remote_repo_path)
  >  (bookmarks ()))
  > EOF
  ((bookmarks_to_rerun ()))

If the feature's tip matches the bookmark, nothing to retry.

  $ rev=$(hg log -r . --template '{node|short}')

  $ simple-sync-state a $rev
  ((bookmarks_to_rerun ()))

Moving the tip of the feature

  $ echo b > a; hg commit -m b
  $ rev2=$(hg log -r . --template '{node|short}')

  $ simple-sync-state a $rev2 Pending_or_working_on_it
  ((bookmarks_to_rerun ()))
  $ simple-sync-state a $rev2 Done
  ((bookmarks_to_rerun (a)))

We retry if we don't get results, but at most twice (including the one above):

  $ simple-sync-state a $rev2 Done
  ((bookmarks_to_rerun (a)))
  $ simple-sync-state a $rev2 Done
  ((bookmarks_to_rerun ()))

We can still force the retry manually:

  $ fe tools force-retry a
  $ simple-sync-state a $rev2 Done
  ((bookmarks_to_rerun (a)))
  $ simple-sync-state a $rev2 Done
  ((bookmarks_to_rerun ()))

Once we get an update, retries work again:

  $ feature_to_server a
  $ simple-sync-state a $rev2 Done
  ((bookmarks_to_rerun ()))
  $ simple-sync-state a $rev Done
  ((bookmarks_to_rerun (a)))

The retry limits are not persistent:

  $ simple-sync-state a $rev Done
  ((bookmarks_to_rerun (a)))
  $ simple-sync-state a $rev Done
  ((bookmarks_to_rerun ()))
  $ fe-server stop
  $ fe-server start
  $ simple-sync-state a $rev Done
  ((bookmarks_to_rerun (a)))
