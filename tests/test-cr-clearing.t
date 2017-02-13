Start test.

  $ start_test

Create repo.

  $ hg init repo
  $ cd repo
  $ touch a
  $ hg add a
  $ hg com -m 'a'
  $ remote=$(pwd)
  $ fe create root -desc root -remote $remote

Add a file with a CR.

  $ cat >file <<EOF
  > # $CR user for user:
  > EOF
  $ hg add file
  $ hg com -m com

Tell the server, and confirm the CR is there.

  $ feature_to_server root -fake-valid-obligations
  $ fe crs root -summary
  |---------------------|
  | user  | CRs | total |
  |-------+-----+-------|
  | user  |   1 |     1 |
  | total |   1 |     1 |
  |---------------------|
  $ fe todo -for user
  |---------------|
  | feature | CRs |
  |---------+-----|
  | root    |   1 |
  |---------------|

Delete the CR.

  $ hg up -q -r root
  $ echo >file
  $ hg com -m com

Tell the server, and confirm that the CR is gone.

  $ feature_to_server root -fake-valid-obligations
  $ fe crs root -summary
  $ fe crs root -for user
  $ fe todo -for user
