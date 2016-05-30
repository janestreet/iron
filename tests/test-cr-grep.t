
Start test.

  $ start_test

Setup a repo with a few crs:

  $ hg init repo
  $ cd repo
  $ remote="$PWD"
  $ cat > file1.ml <<EOF
  > (* $CR user1 for user2: 1/2 *)
  > (* $CR user1 for user2: 2/2 *)
  > (* $CR-soon user1 for user2: 1/2 *)
  > (* $CR-soon user1 for user2: 2/2 *)
  > (* $CR-someday user1 for user2: 1/2 *)
  > (* $CR-someday user1 for user2: 2/2 *)
  > (* $CR user2 for user1: user2 *)
  > (* $XCR user1 for user2: Xed *)
  > (* $CR user1: unassigned *)
  > EOF
  $ mkdir .fe; touch .fe/obligations-repo.sexp .fe/obligations-global.sexp
  $ hg add file1.ml .fe/obligations-repo.sexp .fe/obligations-global.sexp
  $ hg commit -m "add crs"

Create the feature.

  $ fe create -tip . test -remote "$remote" -desc 'root for test'

Check for invalid flag combinations.

  $ fe crs -grep -soon -owner user1
  -owner is incompatible with -soon/someday.
  [1]

  $ fe crs -grep -soon -summary
  -summary is incompatible with -soon/someday.
  [1]

  $ fe crs -grep -soon -xcrs-only
  -xcrs-only is incompatible with -soon/someday.
  [1]

  $ fe crs -grep -include-active-cr-soons
  -include-active-cr-soons is only meaningful with -soon.
  [1]

  $ fe crs -someday
  [fe crs] is incompatible with -someday, use [fe crs -grep -someday].
  [1]

Test -grep.

  $ fe crs -grep
  file1.ml:9:1:
    CR user1: unassigned 
  $ fe crs -grep -for user2
  file1.ml:1:1:
    CR user1 for user2: 1/2 
  
  file1.ml:2:1:
    CR user1 for user2: 2/2 
  $ fe crs -grep -for user1
  file1.ml:7:1:
    CR user2 for user1: user2 
  
  file1.ml:8:1:
    XCR user1 for user2: Xed 
  $ fe crs -grep -for user1 -owner user1
  file1.ml:7:1:
    CR user2 for user1: user2 
  
  file1.ml:8:1:
    XCR user1 for user2: Xed 
  
  file1.ml:9:1:
    CR user1: unassigned 
  $ fe crs -grep -for user1 -xcrs-only
  file1.ml:8:1:
    XCR user1 for user2: Xed 

Test -grep -soon.

  $ fe crs -grep -soon
  $ fe crs -grep -soon -for user2
  file1.ml:3:1:
    CR-soon user1 for user2: 1/2 
  
  file1.ml:4:1:
    CR-soon user1 for user2: 2/2 
  $ fe crs -grep -soon -for user1

Test -grep -someday.

  $ fe crs -grep -someday
  $ fe crs -grep -someday -for user2
  file1.ml:5:1:
    CR-someday user1 for user2: 1/2 
  
  file1.ml:6:1:
    CR-someday user1 for user2: 2/2 
  $ fe crs -grep -someday -for user1

Test -grep -soon -someday.

  $ fe crs -grep -soon -someday
  $ fe crs -grep -soon -someday -for user2
  file1.ml:3:1:
    CR-soon user1 for user2: 1/2 
  
  file1.ml:4:1:
    CR-soon user1 for user2: 2/2 
  
  file1.ml:5:1:
    CR-someday user1 for user2: 1/2 
  
  file1.ml:6:1:
    CR-someday user1 for user2: 2/2 
  $ fe crs -grep -soon -someday -for user1

Test -grep -summary.

  $ fe crs -grep -summary
  |---------------------------------------------|
  | user                   | CRs | XCRs | total |
  |------------------------+-----+------+-------|
  | user2                  |   2 |      |     2 |
  | user1                  |   1 |    1 |     2 |
  | unix-login-for-testing |   1 |      |     1 |
  | total                  |   4 |    1 |     5 |
  |---------------------------------------------|
