Start test.

  $ start_test

Make a repo with root and child features, both permanent.

  $ hg init repo
  $ cd repo
  $ remote=$PWD
  $ touch file; hg add file; hg com -m 0
  $ fe create root       -d root  -permanent -remote $remote
  $ feature_to_server root       -fake-valid
  $ fe enable
  $ fe second -even-though-owner -even-though-empty
  $ fe create root/child -d child -permanent
  $ fe enable
  $ echo change >file; hg com -m 2
  $ feature_to_server root/child -fake-valid

Review child.

  $ u=unix-login-for-testing
  $ fe change -add-whole-feature-reviewers seconder
  $ IRON_USER=seconder fe second
  $ fe tools mark-fully-reviewed root/child -for all -reason reason
  $ fe catch-up mark-file root/child file -for seconder

Release child.

  $ fe release

No review remains, even before Iron server hears from hydra.

  $ fe todo
  Features you own:
  |--------------------------------------------|
  | feature | base    | #left | next step      |
  |---------+---------+-------+----------------|
  | root    |         |       |                |
  |   child | pending | error | wait for hydra |
  |--------------------------------------------|
  $ fe show root/child
  root/child
  ==========
  child
  
  |------------------------------------------------------------|
  | attribute               | value                            |
  |-------------------------+----------------------------------|
  | next step               | wait for hydra                   |
  | owner                   | unix-login-for-testing           |
  | whole-feature reviewers | seconder, unix-login-for-testing |
  | seconder                | seconder                         |
  | review is enabled       | true                             |
  | reviewing               | all                              |
  | is permanent            | true                             |
  | bookmark update         | pending for *                | (glob)
  | tip                     | cee6d61471f1                     |
  | base                    | cee6d61471f1                     |
  | base facts              | pending for *                | (glob)
  | base is ancestor of tip | pending for *                | (glob)
  |------------------------------------------------------------|
  
  ("not showing line counts"
   ("line count is not knowable" ("pending for" *))) (glob)

Hydra informs Iron server.

  $ feature_to_server root/child -fake-valid
  $ feature_to_server root       -fake-valid

No review remains.

  $ fe show root/child
  root/child
  ==========
  child
  
  |------------------------------------------------------------|
  | attribute               | value                            |
  |-------------------------+----------------------------------|
  | next step               | add code                         |
  | owner                   | unix-login-for-testing           |
  | whole-feature reviewers | seconder, unix-login-for-testing |
  | seconder                | seconder                         |
  | review is enabled       | true                             |
  | reviewing               | all                              |
  | is permanent            | true                             |
  | tip                     | cee6d61471f1                     |
  | base                    | cee6d61471f1                     |
  |------------------------------------------------------------|

