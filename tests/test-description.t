Start test.

  $ start_test

Make a repo with a root feature.

  $ hg init repo
  $ cd repo
  $ remote=$PWD
  $ touch file; hg add file; hg com -m 0
  $ rev0=$(hg log -r . --template {node})
  $ fe create root -remote $remote -d root
  $ feature_to_server root -fake-valid

  $ id=$(fe show -id)

Show the description.

  $ fe description show
  root

Set the description.

  $ fe description set <<EOF
  > A new description
  > # with a comment in it
  > EOF

Show the new description.

  $ fe description show
  A new description
  # with a comment in it

Set to the same description, but with leading and trailing whitespace.

  $ fe description set <<EOF
  > 
  > A new description
  > # with a comment in it
  > 
  > EOF

The whitespace has been stripped.

  $ fe description show
  A new description
  # with a comment in it

Archived the feature, and one can still show the description.

  $ fe archive root

  $ fe description show ${id}
  A new description
  # with a comment in it

  $ fe description show root -archived
  A new description
  # with a comment in it
