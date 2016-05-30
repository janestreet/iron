Start test.

  $ start_test

Setup a repository with a CR-soon for a user that otherwise does not exist.

  $ hg init repo
  $ cd repo
  $ remote="$PWD"
  $ touch file; hg add file; hg com -m 0
  $ fe create root -remote $remote -desc root
  $ u=this-user-has-only-this-cr-soon
  $ cat >file <<EOF
  > (* $CR-soon user for $u: cr1 *)
  > EOF
  $ hg commit -m 1
  $ feature_to_server root -fake-valid-obligations

The user sees the CR-soon.

  $ fe todo -for $u
  CR-soons assigned to you:
  |--------------------|
  | family | file:line |
  |--------+-----------|
  | root   | file:1    |
  |--------------------|
