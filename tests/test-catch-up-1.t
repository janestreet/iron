Start test.

  $ start_test

Setup a repo with two files:

  $ hg init repo
  $ cd repo
  $ touch a b; hg add a b
  $ hg commit -m "init"
  $ seq 1 3 > a
  $ seq 1 5 > b
  $ hg commit -m "add some lines"

It is an error to catch up nonexistent features.

  $ fe internal catch-up show-num-lines test
  ("no such feature" test)
  [1]

Create the feature.

  $ fe create -tip . test -remote "$PWD" -desc 'root for test'
  $ feature_to_server test -fake-valid-obligations

It is an error to catch-up a file that is not pending for catch up.

  $ fe catch-up mark-file test a |& matches "catch up is up to date"
  [1]

Enable.

  $ fe enable-review test -add-whole-feature-reviewers user1,user2
  $ fe change test -set-reviewing-all

Review & Catch up.

  $ fe internal session show-num-lines test  -for user1
  8
  $ fe internal catch-up show-num-lines test -for user1
  0
  $ fe session mark-file test a     -for user1 -reason reason
  $ fe internal session show-num-lines test  -for user1
  5
  $ completion-test fe review t
  test
  $ fe internal catch-up show-num-lines test -for user1
  3
  $ session_id=$(fe catch-up show -for user1 -session-id)
  $ fe catch-up is-needed
  false
  $ fe catch-up is-needed -for user1
  true
  $ fe catch-up show -for user1
  test
  ====
  root for test
  
  |----------------------------------------------------------------|
  | attribute               | value                                |
  |-------------------------+--------------------------------------|
  | is archived             | false                                |
  | owner                   | unix-login-for-testing               |
  | whole-feature reviewers | unix-login-for-testing, user1, user2 |
  | seconder                | not seconded                         |
  | is permanent            | false                                |
  | tip                     | f610d97eacad                         |
  | base                    | 9ecad0f82c66                         |
  |----------------------------------------------------------------|
  
  Reviewing test to f610d97eacad.
  1 files to review: 3 lines total
  
  Catch-up.  unix-login-for-testing reviewed this for you, giving the reason as:
  reason
     [ ] 3 a
  $ fe catch-up diff -for user1 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ a @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny level10
  base 9ecad0f82c66 | tip f610d97eacad
  @@@@@@@@ base 1,1 tip 1,4 @@@@@@@@
  +|1
  +|2
  +|3
  $ fe catch-up diff -for user1 -session-id $session_id >/dev/null
  $ fe catch-up diff -for user1 -session-id $(echo $session_id | sed 's/[a-f0-9]/0/g') \
  >     |& matches "incorrect review-session id"
  [1]
  $ echo q | fe review -only-catch-up-review test -for user1
  test
  ====
  root for test
  
  |----------------------------------------------------------------|
  | attribute               | value                                |
  |-------------------------+--------------------------------------|
  | is archived             | false                                |
  | owner                   | unix-login-for-testing               |
  | whole-feature reviewers | unix-login-for-testing, user1, user2 |
  | seconder                | not seconded                         |
  | is permanent            | false                                |
  | tip                     | f610d97eacad                         |
  | base                    | 9ecad0f82c66                         |
  |----------------------------------------------------------------|
  
  Reviewing test to f610d97eacad.
  1 files to review: 3 lines total
  
  Catch-up.  unix-login-for-testing reviewed this for you, giving the reason as:
  reason
     [ ] 3 a
  Quit

Completion of a feature with both review and catch-up, when it has children
without catch-up:

  $ fe create test/foo
  $ completion-test fe review t
  test/
  $ fe archive test/foo
  $ hg up -r test > /dev/null

Persistence.

  $ fe-server stop
  $ fe-server start
  $ fe internal session show-num-lines test  -for user1
  5
  $ fe internal catch-up show-num-lines test -for user1
  3
  $ fe session mark-file test a     -for user1 -reason reason \
  >     |& matches "already reviewed"
  [1]

Check that mark-fully-reviewed also triggers some catch-up.

  $ fe tools mark-fully-reviewed test -for user1 -reason reason
  $ fe internal session show-num-lines test -for user1
  0
  $ fe internal catch-up show-num-lines test -for user1
  8

No catch up if reviewed as yourself.

  $ fe session mark-file test a
  $ fe internal session show-num-lines test
  5
  $ fe internal catch-up show-num-lines test
  0
  $ fe session mark-file test b
  $ fe internal session show-num-lines test
  0
  $ fe internal catch-up show-num-lines test
  0

See if catch-up state persists across an archive.
Let's bounce the server after the archive to check persistence here, too.

  $ fe internal session show-num-lines test  -for user2
  8
  $ fe internal catch-up show-num-lines test -for user2
  0
  $ fe session mark-file test a     -for user2 -reason reason
  $ fe session mark-file test b     -for user2 -reason reason
  $ fe internal session show-num-lines test  -for user2
  0
  $ fe internal catch-up show-num-lines test -for user2
  8

Catch up on archived features.

  $ fe archive test
  $ fe internal session show-num-lines test  -for user2
  ("no such feature" test)
  [1]
  $ fe internal catch-up show-num-lines test -for user2
  8
  $ fe-server stop
  $ fe-server start
  $ fe internal session show-num-lines test  -for user2
  ("no such feature" test)
  [1]
  $ fe internal catch-up show-num-lines test -for user2
  8
  $ fe catch-up mark-file test a    -for user2
  $ fe internal catch-up show-num-lines test -for user2
  5

Completion and partial names for catch up on archived features.

  $ completion-test fe review t
  test
  $ fe internal catch-up show-num-lines t -for user2
  5

One can clear all catch-up for a given feature.

  $ fe internal catch-up show-num-lines test -for user1
  8

  $ fe catch-up clear test -for invalid-user-name
  (error (clear-catch-up-sessions ("never heard of user" invalid-user-name)))
  [1]

  $ fe catch-up clear test -for user1 -only-those-reviewed-by invalid-user-name
  (error (clear-catch-up-sessions ("never heard of user" invalid-user-name)))
  [1]

  $ fe catch-up clear test -for user1 -only-those-reviewed-by user2
  (error
   (clear-catch-up-sessions
    ("no catch-up to clear"
     ((feature_path test) (for_ user1) (only_those_reviewed_by user2)))))
  [1]

  $ fe catch-up clear test -for user1 -only-those-reviewed-by user2 \
  >   -ok-if-nothing-cleared

  $ fe internal catch-up show-num-lines test -for user1
  8

  $ fe catch-up clear test -for user1 \
  >   -only-those-reviewed-by '(or unix-login-for-testing false)'

  $ fe internal catch-up show-num-lines test -for user1
  0

  $ fe catch-up show test -for user1
  No review to catch up on for user1 in test.
  [1]

  $ fe catch-up diff test -for user1
  No review to catch up on for user1 in test.
  [1]
