
Start test.

  $ start_test

Setup a repo with a few crs:

  $ hg init repo
  $ cd repo
  $ remote="$PWD"
  $ touch file1.ml
  $ hg add file1.ml
  $ hg commit -m 'init'
  $ cat > file1.ml <<EOF
  > (* $CR user1 for user2: 1/2 *)
  > (* $CR user1 for user2: 2/2 *)
  > (* $CR user2 for user1: user2 *)
  > (* $XCR user1 for user2: Xed *)
  > (* $CR user1: unassigned *)
  > EOF
  $ hg commit -m "add crs"
  $ hg bookmark test

It is an error to check crs for nonexistent features.

  $ fe crs test
  ("no such feature" test)
  [1]

  $ fe crs test -for all
  ("no such feature" test)
  [1]

Create the feature.

  $ fe create -no-bookmark -tip . test -remote "$remote" -desc 'root for test'
  $ feature_to_server test -fake-valid-obligations
  $ fe list
  |-----------------------------|
  | feature | lines | next step |
  |---------+-------+-----------|
  | test    |     5 | CRs       |
  |-----------------------------|

Get the expected crs.

  $ fe crs non-existent-feature
  ("no such feature" non-existent-feature)
  [1]

  $ fe crs test -for file-owner
  $ fe crs -for user1
  file1.ml:3:1:
    CR user2 for user1: user2 
  
  file1.ml:4:1:
    XCR user1 for user2: Xed 

  $ fe crs -for user1 -owner user1
  file1.ml:3:1:
    CR user2 for user1: user2 
  
  file1.ml:4:1:
    XCR user1 for user2: Xed 

  $ fe crs -for user2
  file1.ml:1:1:
    CR user1 for user2: 1/2 
  
  file1.ml:2:1:
    CR user1 for user2: 2/2 

  $ fe crs -summary
  |---------------------------------------------|
  | user                   | CRs | XCRs | total |
  |------------------------+-----+------+-------|
  | user2                  |   2 |      |     2 |
  | user1                  |   1 |    1 |     2 |
  | unix-login-for-testing |   1 |      |     1 |
  | total                  |   4 |    1 |     5 |
  |---------------------------------------------|
  $ fe crs -xcrs-only -for user1
  file1.ml:4:1:
    XCR user1 for user2: Xed 
