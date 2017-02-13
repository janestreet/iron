Start test.

  $ start_test

Setup a repository with an unassigned CR.

  $ hg init repo
  $ cd repo
  $ touch file
  $ hg add file
  $ hg commit -m '0'
  $ r0=$(hg tip --template={node})
  $ echo >file "(* $CR user: text *)"
  $ hg commit -m '1'
  $ r1=$(hg tip --template={node})
  $ remote="$PWD"

Create feature and feed to hydra.

  $ fe create root -desc root -base $r0 -tip $r1 -remote $remote
  $ feature_to_server root -fake-valid-obligations

The unassigned CR is assigned to the feature owner.

  $ fe crs -for unix-login-for-testing
  file:1:1:
    CR user: text 
  $ fe crs -for new-owner
  $ fe crs -summary
  |--------------------------------------|
  | user                   | CRs | total |
  |------------------------+-----+-------|
  | unix-login-for-testing |   1 |     1 |
  | total                  |   1 |     1 |
  |--------------------------------------|
  $ fe todo -for unix-login-for-testing
  |---------------|
  | feature | CRs |
  |---------+-----|
  | root    |   1 |
  |---------------|
  
  Features you own:
  |-----------------------------------|
  | feature | CRs | #left | next step |
  |---------+-----+-------+-----------|
  | root    |   1 |     1 | CRs       |
  |-----------------------------------|
  $ fe todo -for new-owner

Change the owner, and the assignment should change.

  $ fe change -set-owner new-owner
  $ fe crs -for unix-login-for-testing
  $ fe crs -for new-owner
  file:1:1:
    CR user: text 
  $ fe crs -summary
  |-------------------------|
  | user      | CRs | total |
  |-----------+-----+-------|
  | new-owner |   1 |     1 |
  | total     |   1 |     1 |
  |-------------------------|
  $ fe todo -for unix-login-for-testing
  $ fe todo -for new-owner
  |---------------|
  | feature | CRs |
  |---------+-----|
  | root    |   1 |
  |---------------|
  
  Features you own:
  |-----------------------------------|
  | feature | CRs | #left | next step |
  |---------+-----+-------+-----------|
  | root    |   1 |     1 | CRs       |
  |-----------------------------------|
