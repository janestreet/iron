  $ start_test

Setup repo with three revs: r0 --> r1 --> r2

| feature    | base | tip |
|------------+------+-----|
| root       | r0   | r2  |
| root/child | r0   | r1  |

  $ setup_repo_and_root file
  $ r0=$(hg tip --template={node})
  $ echo change >file; hg com -m change
  $ r1=$(hg tip --template='{node}')
  $ echo change2 >file; hg com -m change2
  $ r2=$(hg tip --template='{node}')
  $ feature_to_server root -fake-valid-obligations
  $ fe create root/child -d root/child -base $r0 -tip $r1
  $ feature_to_server root/child -fake-valid-obligations

Rebasing fails because root/child is included in root.

  $ fe rebase |& matches "feature's tip is already an ancestor of new base"
  [1]

But it succeeds if root/child is empty.

  $ fe change root/child -set-base $r1
  $ feature_to_server root/child -fake-valid-obligations
  $ fe rebase
