  $ start_test

Setup feature with review in two files

  $ setup_repo_and_root file1 file2
  $ echo start >file2; hg com -m change
  $ attribute="((owner user1)(file_reviewers (user1)))"
  $ function current-rev { hg log -r . --template {node}; }
  $ attributes="($(fe show -base) $attribute) ($(current-rev) $attribute)"
  $ feature_to_server root -fake-valid -fake-attribute-by-rev "$attributes"
  $ fe enable
  $ fe second -even-though-owner
  $ fe tools mark-fully-reviewed root
  $ IRON_USER=user1 fe tools mark-fully-reviewed root
  $ CHECK_POINT=$(fe show -tip root)
  $ echo change >file1; echo change >file2; hg com -m change
  $ attributes="$attributes ($(current-rev) $attribute)"
  $ feature_to_server root -fake-valid -fake-attribute-by-rev "$attributes"

  $ fe show
  root
  ====
  root
  
  |---------------------------------------------------------------------|
  | attribute              | value                                      |
  |------------------------+--------------------------------------------|
  | next step              | review                                     |
  | owner                  | unix-login-for-testing                     |
  | whole-feature reviewer | unix-login-for-testing                     |
  | seconder               | unix-login-for-testing (even though owner) |
  | review is enabled      | true                                       |
  | reviewing              | all                                        |
  | is permanent           | true                                       |
  | tip                    | * | (glob)
  | base                   | * | (glob)
  |---------------------------------------------------------------------|
  
  |---------------------------------------------|
  | user                   | review | completed |
  |------------------------+--------+-----------|
  | unix-login-for-testing |      4 |         2 |
  | user1                  |      4 |         2 |
  |---------------------------------------------|

Review one file, but not the other.

  $ fe session mark-file root file1
  $ IRON_USER=user1 fe session mark-file root file1

Revert the feature to the check point, and it's not releasable.

  $ hg revert . -q -r ${CHECK_POINT} ; hg com -m 'revert to check point'
  $ attributes="$attributes ($(current-rev) $attribute)"
  $ feature_to_server root -fake-valid -fake-attribute-by-rev "$attributes"

  $ fe show -omit-attribute-table -omit-description
  root
  ====
  
  |------------------------------------------------------|
  | user                   | review | follow | completed |
  |------------------------+--------+--------+-----------|
  | unix-login-for-testing |      6 |        |         4 |
  | user1                  |      4 |      2 |         4 |
  |------------------------------------------------------|
  
  |-----------------------------|
  | review sessions in progress |
  |-----------------------------|
  | unix-login-for-testing      |
  | user1                       |
  |-----------------------------|

  $ fe show -next-steps
  (Review)

unix-login-for-testing forgets the session and the feature isn't yet releasable.

  $ SESSION_ID=$(fe session show -id)
  $ fe session lock
  $ fe session forget -session-id ${SESSION_ID} -file file1
  $ fe session commit -session-id ${SESSION_ID}
  $ fe show -next-steps
  (Review)

  $ fe show -omit-attribute-table -omit-description
  root
  ====
  
  |------------------------------------------------------|
  | user                   | review | follow | completed |
  |------------------------+--------+--------+-----------|
  | user1                  |      4 |      2 |         4 |
  | unix-login-for-testing |        |        |         2 |
  |------------------------------------------------------|
  
  |-----------------------------|
  | review sessions in progress |
  |-----------------------------|
  | user1                       |
  |-----------------------------|

user1 forgets the session and the feature becomes releasable.

  $ SESSION_ID=$(fe session show -id -for user1)
  $ IRON_USER=user1 fe session lock
  $ IRON_USER=user1 fe session forget -session-id ${SESSION_ID} -file file1
  $ IRON_USER=user1 fe session commit -session-id ${SESSION_ID}
  $ fe show -next-steps
  (Release)

  $ fe release
  $ fe todo -for user1

A similar test in the case of k-of-n is in [test-relax-partial-review.t].
