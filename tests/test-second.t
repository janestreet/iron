Start test.

  $ start_test

Create hg repo.

  $ mkdir repo
  $ cd repo
  $ remote=$(pwd)
  $ hg init
  $ touch file
  $ hg add file
  $ hg com -m file

Create feature.

  $ fe create root -description root -remote-repo-path $remote
  $ fe change -add-whole-feature-reviewer user1
  $ feature_to_server root -fake-valid-obligations
  $ fe enable-review root
  $ fe change -set-reviewing-whole-feature-only

Unseconding a not seconded feature fails.

  $ fe unsecond |& matches 'the feature is not seconded'
  [1]

Cannot second an empty feature.

  $ fe todo -for seconder
  $ IRON_USER=seconder fe second -interactive false |& matches "feature is empty"
  [1]

Make the feature nonempty.

  $ echo hello >file
  $ hg com -m change
  $ feature_to_server root -fake-valid-obligations

Feature owner cannot second.

  $ fe todo -for seconder
  $ fe second root |& matches "please have a non-owner second"
  [1]

Non-whole-feature reviewer cannot second.

  $ fe todo -for seconder
  $ IRON_USER=some-random-user fe second |& matches "not a whole-feature reviewer"
  [1]
  $ fe change root -add-whole-feature-reviewer seconder

Second.

  $ fe show root -reviewing
  "whole-feature reviewers"
  $ fe todo -for seconder
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |      1 |
  |------------------|
  $ fe tools mark-fully-reviewed root -for seconder -reason reason
  $ fe todo -for seconder
  |--------------------|
  | feature | catch-up |
  |---------+----------|
  | root    |        1 |
  |--------------------|
  $ fe tools mark-fully-reviewed root -for unix-login-for-testing
  $ fe todo -for seconder
  |--------------------|
  | feature | catch-up |
  |---------+----------|
  | root    |        1 |
  |--------------------|
  $ fe tools mark-fully-reviewed root -for user1 -reason reason

All whole-feature reviewers have reviewed, so we now recommend seconding.

  $ fe todo -for seconder
  |--------------------------------|
  | feature | catch-up | next step |
  |---------+----------+-----------|
  | root    |        1 | second    |
  |--------------------------------|
  $ fe second |& matches 'please have a non-owner second'
  [1]
  $ IRON_USER=seconder fe second -even-though-owner \
  >     |& matches 'cannot use -even-though-owner with a non-owner'
  [1]
  $ IRON_USER=seconder fe second -even-though-empty \
  >     |& matches 'cannot use -even-though-empty with nonempty feature'
  [1]
  $ IRON_USER=seconder fe second -interactive false
  $ fe show root -reviewing
  all

Seconding again fails.

  $ IRON_USER=seconder fe second -interactive false |& matches "already seconded"
  [1]

Only a user with a review manager can unsecond.

  $ fe unsecond -for jdoe37
  (error (unsecond (Failure "jdoe37 is not allowed to unsecond")))
  [1]

Unsecond the feature.

  $ fe unsecond -for seconder

Now reviewing is restricted to whole feature reviewers.

  $ fe show
  root
  ====
  root
  
  |-------------------------------------------------------------------|
  | attribute               | value                                   |
  |-------------------------+-----------------------------------------|
  | next step               | ask seconder                            |
  | owner                   | unix-login-for-testing                  |
  | whole-feature reviewers | seconder, unix-login-for-testing, user1 |
  | seconder                | not seconded                            |
  | review is enabled       | true                                    |
  | reviewing               | whole-feature reviewers                 |
  | is permanent            | false                                   |
  | tip                     | 46fab6465033                            |
  | base                    | 04da3968e088                            |
  |-------------------------------------------------------------------|
  
  |-----------------------------------------------|
  | user                   | catch-up | completed |
  |------------------------+----------+-----------|
  | seconder               |        1 |         1 |
  | user1                  |        1 |         1 |
  | unix-login-for-testing |          |         1 |
  |-----------------------------------------------|

Cannot second when Second has been locked.

  $ fe lock -second root -reason testing
  $ fe todo -owned-by-me
  Features you own:
  |-------------------------|
  | feature | next step     |
  |---------+---------------|
  | root    | unlock second |
  |-------------------------|
  $ fe show root -next-step
  ((Unlock Second))
  $ IRON_USER=seconder fe second root \
  >     |& matches "feature lock is locked.*(feature_path root) (lock_name Second).*"
  [1]
  $ fe unlock -second root

Second as an owner.

  $ fe second -even-though-owner
  $ fe show
  root
  ====
  root
  
  |----------------------------------------------------------------------|
  | attribute               | value                                      |
  |-------------------------+--------------------------------------------|
  | next step               | release                                    |
  | owner                   | unix-login-for-testing                     |
  | whole-feature reviewers | seconder, unix-login-for-testing, user1    |
  | seconder                | unix-login-for-testing (even though owner) |
  | review is enabled       | true                                       |
  | reviewing               | all                                        |
  | is permanent            | false                                      |
  | tip                     | 46fab6465033                               |
  | base                    | 04da3968e088                               |
  |----------------------------------------------------------------------|
  
  |-----------------------------------------------|
  | user                   | catch-up | completed |
  |------------------------+----------+-----------|
  | seconder               |        1 |         1 |
  | user1                  |        1 |         1 |
  | unix-login-for-testing |          |         1 |
  |-----------------------------------------------|

Cannot lock Second if the feature is seconded.

  $ fe lock -second root -reason testing
  ("there were problems" ((Second (Error "Feature is already seconded."))))
  [1]

Second -even-though-empty.

  $ fe unsecond
  $ fe change -set-base $(fe show -tip)
  $ feature_to_server root -fake-valid
  $ IRON_USER=seconder fe second -interactive false |& matches "feature is empty"
  [1]
  $ IRON_USER=seconder fe second -interactive false -even-though-empty
