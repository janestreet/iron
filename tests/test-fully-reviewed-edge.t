Start test.

  $ start_test

Setup repo.

  $ setup_repo_and_root file
  $ feature_to_server root -fake-valid
  $ fe enable
  $ fe second -even-though-owner -even-though-empty
  $ rev0=$(hg log -r . --template {node})
  $ echo change >file; hg com -m change
  $ rev1=$(hg log -r . --template {node})
  $ echo change2 >file; hg com -m change2
  $ rev2=$(hg log -r . --template {node})

Add a fully-reviewed edge.

  $ fe tools fully-reviewed-edge check $rev0 $rev1 |& matches "not a fully-reviewed edge"
  [1]
  $ fe internal fully-reviewed-edge add -from $rev0 -to $rev1
  $ fe tools fully-reviewed-edge check $rev0 $rev1

Can't add an edge for an unreleasable feature.

  $ fe tools fully-reviewed-edge check $rev1 $rev2 |& matches "not a fully-reviewed edge"
  [1]
  $ fe create root/child -base $rev1 -tip $rev2 -desc child
  $ feature_to_server root/child -fake-valid
  $ fe enable
  $ fe second -even-though-owner
  $ fe tools fully-reviewed-edge add |& matches "feature is not fully reviewed"
  [1]
  $ fe tools mark-fully-reviewed root/child -for all -reason reason

Add an edge for a fully-reviewed, but unrebased, feature.

  $ fe is-releasable |& matches "feature is not releasable"
  [1]
  $ fe lock -release -reason 'testing'
  $ fe tools fully-reviewed-edge add
  (error
   (add-fully-reviewed-edge
    ("release is locked -- consider using -even-if-release-is-locked"
     root/child)))
  [1]
  $ fe tools fully-reviewed-edge add -even-if-release-is-locked
  $ fe unlock -release
  $ fe tools fully-reviewed-edge check $rev1 $rev2

Persistence.

  $ fe-server stop
  $ fe-server start
  $ fe tools fully-reviewed-edge check $rev0 $rev1
  $ fe tools fully-reviewed-edge check $rev1 $rev2

[fe internal set-brains-to-goal-if-edge]

  $ fe show -next-step
  (Rebase Release)
  $ fe internal set-brains-to-goal-if-edge
  $ fe show -next-step
  (Rebase Release)
  $ fe brain forget -all
  $ fe show -next-step
  (Review)
  $ fe internal set-brains-to-goal-if-edge
  $ fe show -next-step
  (Rebase Release)
