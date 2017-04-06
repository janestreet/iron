Start test.

  $ start_test

Make a repo with a feature.

  $ hg init repo
  $ cd repo
  $ remote=$PWD
  $ touch file; hg add file; hg com -m 0
  $ rev0=$(hg log -r . --template {node})
  $ fe create root -remote $remote -d root
  $ echo change >file; hg com -m 1
  $ rev1=$(hg log -r . --template {node})
  $ feature_to_server root -fake-valid

  $ fe enable-review

Review to do.

  $ fe todo
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |      1 |
  |------------------|
  
  Features you own:
  |-----------------------------|
  | feature | #left | next step |
  |---------+-------+-----------|
  | root    |     1 | review    |
  |-----------------------------|

Mark, and then no review to do.

  $ fe tools mark-fully-reviewed root -for unix-login-for-testing
  $ fe todo
  |----------------------------|
  | feature | next step        |
  |---------+------------------|
  | root    | add w-f-reviewer |
  |----------------------------|
  
  Features you own:
  |----------------------------|
  | feature | next step        |
  |---------+------------------|
  | root    | add w-f-reviewer |
  |----------------------------|

Add some whole-feature reviewers.

  $ fe change -add-whole-feature-reviewers user1,user2,user3
  $ fe show -omit-attribute-table
  root
  ====
  root
  
  |---------------------------------------------|
  | user                   | review | completed |
  |------------------------+--------+-----------|
  | user1                  |      1 |           |
  | user2                  |      1 |           |
  | user3                  |      1 |           |
  | unix-login-for-testing |        |         1 |
  |---------------------------------------------|

Mark one of them.

  $ fe tools mark-fully-reviewed root -for user1 -reason reason
  $ fe show -omit-attribute-table
  root
  ====
  root
  
  |--------------------------------------------------------|
  | user                   | review | catch-up | completed |
  |------------------------+--------+----------+-----------|
  | user2                  |      1 |          |           |
  | user3                  |      1 |          |           |
  | user1                  |        |        1 |         1 |
  | unix-login-for-testing |        |          |         1 |
  |--------------------------------------------------------|

Mark the rest.

  $ fe tools mark-fully-reviewed root -for all -reason reason
  $ fe show -omit-attribute-table
  root
  ====
  root
  
  |-----------------------------------------------|
  | user                   | catch-up | completed |
  |------------------------+----------+-----------|
  | user1                  |        1 |         1 |
  | user2                  |        1 |         1 |
  | user3                  |        1 |         1 |
  | unix-login-for-testing |          |         1 |
  |-----------------------------------------------|

Mark -for-all-but.

  $ fe change -add-whole-feature-reviewers jdoe1,jdoe2
  $ fe show -omit-attribute-table
  root
  ====
  root
  
  |--------------------------------------------------------|
  | user                   | review | catch-up | completed |
  |------------------------+--------+----------+-----------|
  | jdoe1                  |      1 |          |           |
  | jdoe2                  |      1 |          |           |
  | user1                  |        |        1 |         1 |
  | user2                  |        |        1 |         1 |
  | user3                  |        |        1 |         1 |
  | unix-login-for-testing |        |          |         1 |
  |--------------------------------------------------------|

  $ fe tools mark-fully-reviewed root -for-all-but jdoe1,jdoe2 -reason reason
  $ fe show -omit-attribute-table
  root
  ====
  root
  
  |--------------------------------------------------------|
  | user                   | review | catch-up | completed |
  |------------------------+--------+----------+-----------|
  | jdoe1                  |      1 |          |           |
  | jdoe2                  |      1 |          |           |
  | user1                  |        |        1 |         1 |
  | user2                  |        |        1 |         1 |
  | user3                  |        |        1 |         1 |
  | unix-login-for-testing |        |          |         1 |
  |--------------------------------------------------------|

  $ fe tools mark-fully-reviewed root -for-all-but jdoe1 -reason reason
  $ fe show -omit-attribute-table
  root
  ====
  root
  
  |--------------------------------------------------------|
  | user                   | review | catch-up | completed |
  |------------------------+--------+----------+-----------|
  | jdoe1                  |      1 |          |           |
  | jdoe2                  |        |        1 |         1 |
  | user1                  |        |        1 |         1 |
  | user2                  |        |        1 |         1 |
  | user3                  |        |        1 |         1 |
  | unix-login-for-testing |        |          |         1 |
  |--------------------------------------------------------|

Cannot mark -for and -for-all-but in one command.

  $ fe tools mark-fully-reviewed root -for all -for-all-but jdoe1 -reason reason
  The flags [-for] and [-for-all-but] are mutually exclusive.
  [1]
