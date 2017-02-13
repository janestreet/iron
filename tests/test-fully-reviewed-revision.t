Start test.

  $ start_test

  $ setup_repo_and_root file
  $ rev0=$(hg log -r . --template {node})
  $ echo change >file; hg com -m change
  $ rev1=$(hg log -r . --template {node})
  $ feature_to_server root -fake-valid
  $ fe tools mark-fully-reviewed root -for all -reason reason
  $ fe enable
  $ fe second -even-though-owner
  $ fe release
  $ feature_to_server root -fake-valid

Nothing is fully reviewed yet.

  $ fe tools fully-reviewed-rev check $rev0 |& matches 'not fully reviewed'
  [1]
  $ fe tools fully-reviewed-rev check $rev1 |& matches 'not fully reviewed'
  [1]

Add a fully-reviewed rev.

  $ fe internal fully-reviewed-rev add $rev0
  $ fe tools fully-reviewed-rev check $rev0
  $ fe tools fully-reviewed-rev check $rev1

Persistence.

  $ fe-server stop
  $ fe-server start
  $ fe tools fully-reviewed-rev check $rev1

Remove a fully-reviewed rev.

  $ fe internal fully-reviewed-rev remove $rev0
  $ fe tools fully-reviewed-rev check $rev1 |& matches 'not fully reviewed'
  [1]
