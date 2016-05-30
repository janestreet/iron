  $ start_test
  $ setup_repo_and_root file

Initially, the feature has a bookmark.

  $ REV12=$(hg id | cut -d ' ' -f 1)

  $ fe show root -has-bookmark
  true

  $ fe todo -bookmarks-without-feature

  $ fe internal rpc-to-server call synchronize-state <<EOF
  > ((remote_repo_path $PWD)
  >  (bookmarks (
  >    ((bookmark root)
  >     (first_12_of_rev $REV12)
  >     (rev_author_or_error (Ok unix-login-for-testing))
  >     (status Done)
  >    )
  >    ((bookmark root/without-feature)
  >     (first_12_of_rev $REV12)
  >     (rev_author_or_error (Ok unix-login-for-testing))
  >     (status Done)
  >    )
  >  )))
  > EOF
  ((bookmarks_to_rerun (root)))

  $ fe todo -bookmarks-without-feature
  root/without-feature

An admin can clear the bookmarks without a feature.

  $ fe internal clear-bookmarks-without-feature $TESTTMP/repo

  $ fe todo -bookmarks-without-feature

  $ fe internal rpc-to-server call synchronize-state <<EOF
  > ((remote_repo_path $PWD)
  >  (bookmarks (
  >    ((bookmark root)
  >     (first_12_of_rev $REV12)
  >     (rev_author_or_error (Ok unix-login-for-testing))
  >     (status Done)
  >    )
  >    ((bookmark root/without-feature)
  >     (first_12_of_rev $REV12)
  >     (rev_author_or_error (Ok unix-login-for-testing))
  >     (status Done)
  >    )
  >  )))
  > EOF
  ((bookmarks_to_rerun (root)))

  $ fe todo -bookmarks-without-feature
  root/without-feature

Archiving the root shall also clear the bookmarks without a feature.

  $ fe change -set-is-permanent false root
  $ fe archive root

  $ fe todo -bookmarks-without-feature
