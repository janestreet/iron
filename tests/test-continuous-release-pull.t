Start test.

  $ start_test

Setup root.

  $ hg init remote
  $ remote=$(cd remote && pwd)
  $ hg clone -q remote repo
  $ cd repo
  $ repo=$(pwd)
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
  $ feature_to_server root/child -fake-valid

Push the local commit and add a commit to the feature in the remote repo.

  $ hg -q push $remote
  $ cd $remote
  $ IRON_OPTIONS='((workspaces false))' fe update root/child
  $ echo remote-change >>file; hg com -m remote-change
  $ feature_to_server root/child -fake-valid

Mark the feature as reviewed (from the remote).

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

Release child from the local repo (which is behind by one commit).

  $ cd $repo
  $ hg log -r f8afa1835c87
  abort: unknown revision 'f8afa1835c87'!
  [255]
  $ fe release
  $ ( cd $remote && hg book )
     [release]root/child       2:f8afa1835c87
     root                      0:0499c217bb29
   * root/child                2:f8afa1835c87
