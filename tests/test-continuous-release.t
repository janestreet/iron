Start test.

  $ start_test

Setup root.

  $ hg init remote
  $ remote=$(cd remote && pwd)
  $ hg clone -q remote repo
  $ cd repo
  $ touch file; hg add file; hg com -m add
  $ fe create root -desc root -remote $remote
  $ feature_to_server root -fake-valid
  $ fe enable-review
  $ fe second -even-though-owner -even-though-empty
  $ fe change -set-who-can-release-into-me my-owners
  $ fe change -set-release-process continuous

Setup child.

  $ fe create root/child -owner user1 -desc child
  $ echo change >file; hg com -m change
  $ hg push -q -r root/child $remote
  $ feature_to_server root/child -fake-valid
  $ fe enable-review
  $ IRON_USER=user1 fe second -even-though-owner
  $ fe tools mark-fully-reviewed root/child -for user1 -reason reason

Show the root attributes.

  $ fe show root
  root
  ====
  root
  
  |---------------------------------------------------------------------|
  | attribute              | value                                      |
  |------------------------+--------------------------------------------|
  | next step              | add code                                   |
  | owner                  | unix-login-for-testing                     |
  | whole-feature reviewer | unix-login-for-testing                     |
  | seconder               | unix-login-for-testing (even though owner) |
  | review is enabled      | true                                       |
  | reviewing              | all                                        |
  | is permanent           | false                                      |
  | tip                    | 0499c217bb29                               |
  | base                   | 0499c217bb29                               |
  | release into me        |                                            |
  |   release process      | continuous                                 |
  |   who can release      | my owners                                  |
  |---------------------------------------------------------------------|

Release failures.

  $ fe release -for user1 |& matches "only these users may release.* (unix-login-for-testing)"
  [1]

Release child.

  $ fe change root -set-who-can-release-into-me my-owners-and-child-owners
  $ fe release -for user1
  $ ( cd ../remote && hg book )
     [release]root/child       1:25b2443be372
     root                      0:0499c217bb29
     root/child                1:25b2443be372

Releasing again is OK, as [user1] or [unix-login-for-testing].

  $ fe release -for user1
  $ fe release

Have hydra release the child directly.

  $ ( cd ../remote && hg book -d '[release]root/child' )
  $ fe release -for as-hydra
  $ fe list -depth max
  |-----------------------------|
  | feature | lines | next step |
  |---------+-------+-----------|
  | root    |     0 | add code  |
  |-----------------------------|
  $ ( cd ../remote && hg book )
     root                      1:25b2443be372
