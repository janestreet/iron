Start test. 

  $ start_test

Setup a repository with a CR-soon.

  $ hg init repo
  $ cd repo
  $ touch file
  $ hg add file
  $ hg commit -m '0'
  $ r0=$(hg tip --template={node})
  $ cat >file <<EOF
  > (* $CR user1 for user2: text *)
  > EOF
  $ hg commit -m '1'
  $ r1=$(hg tip --template={node})

Create the root feature r0->r1.

  $ remote="$PWD"
  $ fe create root -desc 'root' -base $r0 -tip $r1 -remote $remote

Feed the root to hydra.

  $ feature_to_server root -fake-valid-obligations

Whats is there to do?

  $ fe todo
  Features you own:
  |-----------------------------------|
  | feature | CRs | #left | next step |
  |---------+-----+-------+-----------|
  | root    |   1 |     1 | CRs       |
  |-----------------------------------|
  $ fe todo -for user2
  |---------------|
  | feature | CRs |
  |---------+-----|
  | root    |   1 |
  |---------------|
