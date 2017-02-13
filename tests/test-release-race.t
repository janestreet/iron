Start test.

  $ start_test

Make a repo with root and two children features.

  $ hg init repo
  $ cd repo
  $ remote=$PWD
  $ touch file; hg add file; hg com -m 0
  $ rev0=$(hg log -r . --template {node})
  $ fe create root -remote $remote -d root -permanent
  $ echo a > file; hg commit -m 1
  $ feature_to_server root -fake-valid
  $ fe enable-review root
  $ fe tools mark-fully-reviewed root
  $ fe second -even-though-owner root

Make a releasable child1 feature.

  $ fe create root/child1 -d child1
  $ feature_to_server root/child1 -fake-valid
  $ echo child1 >>file ; hg com -m 2 &>/dev/null
  $ rev1=$(hg log -r . --template {node})
  $ feature_to_server root/child1 -fake-valid
  $ fe enable-review
  $ fe second -even-though-owner
  $ fe tools mark-fully-reviewed root/child1
  $ fe is-releasable

Make a releasable child2 feature.

  $ fe create root/child2 -d child2
  $ feature_to_server root/child2 -fake-valid
  $ echo child2 >>file ; hg com -m 2 &>/dev/null
  $ rev2=$(hg log -r . --template {node})
  $ feature_to_server root/child2 -fake-valid
  $ fe enable-review
  $ fe second -even-though-owner
  $ fe tools mark-fully-reviewed root/child2
  $ fe is-releasable

If hydra tells fe about a different revision, it prevents releasability.

  $ child1=$(fe show root/child1 -tip)
  $ simple-sync-state root/child1 0123456789ab
  ((bookmarks_to_rerun (root/child1)))
  $ fe is-releasable root/child1
  (error
   (is-releasable
    ("feature is not releasable"
     ((feature root/child1) (next_steps (Wait_for_hydra))))))
  [1]
  $ simple-sync-state root/child1 ${child1:0:12}
  ((bookmarks_to_rerun ()))

Synchronize again.

  $ feature_to_server root        -fake-valid
  $ feature_to_server root/child1 -fake-valid
  $ feature_to_server root/child2 -fake-valid

Look for releasable features in the todo.

  $ fe todo -releasable-names
  root
  root/child1
  root/child2

Attempt to release both features in sequence.  The second release shall fail.

  $ fe release root/child1
  $ fe release root/child2
  ("Failed to release feature" root/child2
   ((feature_base 88de6830d27e) (parent_tip 4b28b03726c3)))
  [1]

  $ function is-ancestor {
  >   ! test -z "$(hg log -l 1 -r $1::$2)"
  > }

  $ is-ancestor ${rev1} root
  $ is-ancestor ${rev2} root
  [1]
