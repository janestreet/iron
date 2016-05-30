  $ start_test

  $ setup_repo_and_root file1 file2
  $ r0=$(hg tip --template={node})
  $ echo change >file1; hg com -m change
  $ feature_to_server root -fake-valid
  $ fe create root/child -d child -base $r0 -tip $r0

Create a local clone.

  $ cd ..
  $ hg clone -q repo local

Extend child in remote repo.

  $ cd repo
  $ fe up root/child
  $ echo 'change' >file2; hg com -q -m change
  $ child_tip=$(hg tip --template={node})
  $ feature_to_server root/child -fake-valid

Rebase in the local repo works.

  $ cd ../local
  $ hg log -r $child_tip 2>/dev/null
  [255]
  $ IRON_OPTIONS='((workspaces false))' fe rebase root/child
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  (branch merge, don't forget to commit)
  $ hg log -r $child_tip >/dev/null
