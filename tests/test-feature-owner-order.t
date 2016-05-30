Start test.

  $ start_test

Make a repo with a root feature.

  $ hg init repo
  $ cd repo
  $ remote=$PWD
  $ touch file; hg add file; hg com -m 0
  $ rev0=$(hg log -r . --template {node})
  $ fe create root -remote $remote -d root
  $ cat >file <<EOF
  > (* $CR user1: *)
  > EOF
  $ hg com -m 1  
  $ rev1=$(hg log -r . --template {node})
  $ feature_to_server root -fake-valid

The first owner gets the CR.

  $ fe change -set-owners a,b
  $ fe show -owners
  (a b)
  $ fe crs -summary
  |---------------------|
  | user  | CRs | total |
  |-------+-----+-------|
  | a     |   1 |     1 |
  | total |   1 |     1 |
  |---------------------|

  $ fe change -set-owners b,a
  $ fe show -owners
  (b a)
  $ fe crs -summary
  |---------------------|
  | user  | CRs | total |
  |-------+-----+-------|
  | b     |   1 |     1 |
  | total |   1 |     1 |
  |---------------------|
