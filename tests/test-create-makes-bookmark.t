  $ start_test

  $ setup_repo_and_root f1.txt
  $ hg book | grep -qF "* root"

Test that 'fe create' fails if the bookmark update failed, at least in
the particular case where the failure was due to a permissions
problem.

  $ cd $TESTTMP
  $ hg init remote
  $ cd remote
  $ echo a > a; hg add a; hg commit -m a
  $ hg book just-to-create-bookmark-file
  $ cd ..
  $ hg clone -q remote local
  $ chmod a-w remote/.hg/bookmarks
  $ cd local
  $ fe create root2 -description root2 -remote-repo-path $TESTTMP/remote
  ("[hg push] failed"
   ((stdout
     ("pushing to $TESTTMP/remote"
      "searching for changes" "no changes found" ""))
    (stderr
     ("transaction abort!" "rollback completed"
      "abort: Permission denied: $TESTTMP/remote/.hg/.bookmarks-*" (glob)
      ""))
    (exit_status (Error (Exit_non_zero 255)))))
  [1]
