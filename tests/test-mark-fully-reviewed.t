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
  CRs and review line counts:
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

  $ fe internal mark-fully-reviewed root -for unix-login-for-testing
  $ fe todo
  Features you own:
  |----------------------------|
  | feature | next step        |
  |---------+------------------|
  | root    | add w-f-reviewer |
  |----------------------------|

Add some whole-feature reviewers.

  $ fe change -add-whole-feature-reviewers user1,user2,user3
  $ fe show
  root
  ====
  root
  
  |-----------------------------------------------------------------------|
  | attribute               | value                                       |
  |-------------------------+---------------------------------------------|
  | next step               | review                                      |
  | owner                   | unix-login-for-testing                      |
  | whole-feature reviewers | unix-login-for-testing, user1, user2, user3 |
  | seconder                | not seconded                                |
  | review is enabled       | true                                        |
  | reviewing               | whole-feature reviewers                     |
  | is permanent            | false                                       |
  | tip                     | 1bb712fa1b87                                |
  | base                    | 6af58578f44e                                |
  |-----------------------------------------------------------------------|
  
  |---------------------------------------------|
  | user                   | review | completed |
  |------------------------+--------+-----------|
  | user1                  |      1 |           |
  | user2                  |      1 |           |
  | user3                  |      1 |           |
  | unix-login-for-testing |        |         1 |
  |---------------------------------------------|

Mark one of them.

  $ fe internal mark-fully-reviewed root -for user1 -reason reason
  $ fe show
  root
  ====
  root
  
  |-----------------------------------------------------------------------|
  | attribute               | value                                       |
  |-------------------------+---------------------------------------------|
  | next step               | review                                      |
  | owner                   | unix-login-for-testing                      |
  | whole-feature reviewers | unix-login-for-testing, user1, user2, user3 |
  | seconder                | not seconded                                |
  | review is enabled       | true                                        |
  | reviewing               | whole-feature reviewers                     |
  | is permanent            | false                                       |
  | tip                     | 1bb712fa1b87                                |
  | base                    | 6af58578f44e                                |
  |-----------------------------------------------------------------------|
  
  |--------------------------------------------------------|
  | user                   | review | catch-up | completed |
  |------------------------+--------+----------+-----------|
  | user2                  |      1 |          |           |
  | user3                  |      1 |          |           |
  | user1                  |        |        1 |         1 |
  | unix-login-for-testing |        |          |         1 |
  |--------------------------------------------------------|

Mark the rest.

  $ fe internal mark-fully-reviewed root -for all -reason reason
  $ fe show
  root
  ====
  root
  
  |-----------------------------------------------------------------------|
  | attribute               | value                                       |
  |-------------------------+---------------------------------------------|
  | next step               | ask seconder                                |
  | owner                   | unix-login-for-testing                      |
  | whole-feature reviewers | unix-login-for-testing, user1, user2, user3 |
  | seconder                | not seconded                                |
  | review is enabled       | true                                        |
  | reviewing               | whole-feature reviewers                     |
  | is permanent            | false                                       |
  | tip                     | 1bb712fa1b87                                |
  | base                    | 6af58578f44e                                |
  |-----------------------------------------------------------------------|
  
  |-----------------------------------------------|
  | user                   | catch-up | completed |
  |------------------------+----------+-----------|
  | user1                  |        1 |         1 |
  | user2                  |        1 |         1 |
  | user3                  |        1 |         1 |
  | unix-login-for-testing |          |         1 |
  |-----------------------------------------------|
