Start test.

  $ start_test

Setup a repo with two files:

  $ copy_sample_repo two-users ./repo
  $ cd repo
  $ hg init
  $ remote="$PWD"
  $ hg add -q .
  $ hg commit -m "init"
  $ seq 1 3 > a
  $ seq 1 5 > b
  $ hg commit -m "add some lines"

Create the feature

  $ fe create -tip . test -remote "$remote" -desc 'root for test'
  $ fe change -add-whole-feature-reviewers user1
  $ feature_to_server test

Enable and review

  $ fe enable-review test

Check that when there is no session yet, providing an id fails.

  $ bogus_id='b04a0291-61bb-302a-a23c-0fd7f5d2d664'

  $ fe session show -session-id ${bogus_id} \
  >     |& matches "no current session"
  [1]

And check that this did not create a session.

  $ fe session show -session-id ${bogus_id} \
  >     |& matches "no current session"
  [1]

Now create a session.

  $ fe internal session show-num-lines test
  8

Requesting an invalid session id fails.

  $ fe session show -session-id ${bogus_id} \
  >     |& matches "incorrect review-session id"
  [1]

  $ ID=$(fe session show -id)
  $ fe session show
  Reviewing test to 9bdd8ea4bdfa.
  2 files to review: 8 lines total
     [ ] 3 a
     [ ] 5 b

One can always manually lock or unlock a session.

  $ fe session lock
  $ fe session show -is-locked
  true

The lock of a session are persisted.

  $ fe-server stop
  $ fe-server start

  $ fe session show -is-locked
  true

  $ fe session unlock
  $ fe session show -is-locked
  false

  $ fe session mark-file test a
  $ fe session show -id | matches ${ID}
  $ fe session show
  Reviewing test to 9bdd8ea4bdfa.
  1 files to review (1 already reviewed): 8 lines total
     [X] 3 a
     [ ] 5 b

It is an error to mark as reviewed a file that is already reviewed, unless a
special switch is supplied.

  $ fe session mark-file test a
  (error (reviewed-diff4 ("This file is already reviewed" (a))))
  [1]

  $ fe session mark-file test a -even-if-some-files-are-already-reviewed

Can get the session even when review is disabled.

  $ fe disable-review
  $ fe session diff | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ a @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base 0f5b3abd13b6 | tip 9bdd8ea4bdfa
  _
  | @@@@@@@@ Hunk 1/2 @@@@@@@@
  | @@@@@@@@ base 1,1 tip 1,4 @@@@@@@@
  | +|1
  | +|2
  | +|3
  |_
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ b @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  _
  | @@@@@@@@ Hunk 2/2 @@@@@@@@
  | @@@@@@@@ base 1,1 tip 1,6 @@@@@@@@
  | +|1
  | +|2
  | +|3
  | +|4
  | +|5
  |_
  $ fe session show
  Reviewing test to 9bdd8ea4bdfa.
  1 files to review (1 already reviewed): 8 lines total
     [X] 3 a
     [ ] 5 b
  $ fe enable-review

Can specify what diff to show.

  $ fe session diff -file a | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ a @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base 0f5b3abd13b6 | tip 9bdd8ea4bdfa
  @@@@@@@@ base 1,1 tip 1,4 @@@@@@@@
  +|1
  +|2
  +|3
  $ fe session diff -unreviewed | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ b @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base 0f5b3abd13b6 | tip 9bdd8ea4bdfa
  @@@@@@@@ base 1,1 tip 1,6 @@@@@@@@
  +|1
  +|2
  +|3
  +|4
  +|5
  $ fe session diff -reviewed | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ a @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base 0f5b3abd13b6 | tip 9bdd8ea4bdfa
  @@@@@@@@ base 1,1 tip 1,4 @@@@@@@@
  +|1
  +|2
  +|3
  $ fe session diff -file c
  ("no such file in session" (c))
  [1]
  $ fe session diff -file a -reviewed \
  >   |& matches "The flags to select which diffs to show are mutually exclusive"
  [1]

Persistence

  $ fe-server stop
  $ fe-server start

  $ fe session show
  Reviewing test to 9bdd8ea4bdfa.
  1 files to review (1 already reviewed): 8 lines total
     [X] 3 a
     [ ] 5 b
  $ fe session show -id | matches ${ID}

Forget (should work outside of a clone):

  $ cd /
  $ fe session forget test -all -session-id ${ID}
  $ fe session show test
  Reviewing test to 9bdd8ea4bdfa.
  2 files to review: 8 lines total
     [ ] 3 a
     [ ] 5 b
  $ cd - > /dev/null

Persistence

  $ fe-server stop
  $ fe-server start
  $ fe session show
  Reviewing test to 9bdd8ea4bdfa.
  2 files to review: 8 lines total
     [ ] 3 a
     [ ] 5 b

Forget specific files

  $ fe session mark-file test a
  $ fe session show
  Reviewing test to 9bdd8ea4bdfa.
  1 files to review (1 already reviewed): 8 lines total
     [X] 3 a
     [ ] 5 b

  $ fe session forget -file b -session-id ${ID} |& matches "not reviewed yet"
  [1]

  $ fe session forget -file a -session-id ${ID}
  $ fe session show
  Reviewing test to 9bdd8ea4bdfa.
  2 files to review: 8 lines total
     [ ] 3 a
     [ ] 5 b

Delete

  $ fe session mark-file test a
  $ fe internal session show-num-lines test
  5

  $ fe session forget -all -session-id ${ID}
  $ fe internal session show-num-lines test
  8

  $ ID=$(fe session show -id)
  $ fe session show
  Reviewing test to 9bdd8ea4bdfa.
  2 files to review: 8 lines total
     [ ] 3 a
     [ ] 5 b

Check that when one has opted in, session are locked by looking at diffs.

  $ fe session unlock
  $ fe session show -is-locked
  false

  $ fe session diff -file a > /dev/null
  $ fe session show -is-locked
  false

  $ fe admin users using-locked-sessions add unix-login-for-testing

  $ fe session show -is-locked
  false
  $ fe session diff -file a > /dev/null
  $ fe session show -is-locked
  true

  $ fe admin users using-locked-sessions remove unix-login-for-testing
  $ fe session unlock

Mark one file.

  $ fe session mark-file test a
  $ fe internal session show-num-lines test
  5

  $ seq 1 10 > b
  $ hg commit -m "more lines in b"
  $ hg bookmark test
  $ BOOKMARK=test fe internal hydra ; hg -q update -r test

  $ fe internal session show-num-lines test
  5

Check that the commands from [admin users set] give proper
error messages.

  $ fe admin users using-locked-sessions add unix-login-for-testing
  $ fe admin users using-locked-sessions add unix-login-for-testing
  (error
   (user-set-change
    ("user already in the set [using-locked-sessions]"
     (unix-login-for-testing))))
  [1]

  $ fe-server stop
  $ fe-server start

  $ fe admin users using-locked-sessions get
  unix-login-for-testing
  $ fe admin users using-locked-sessions remove unix-login-for-testing
  $ fe admin users using-locked-sessions remove unix-login-for-testing
  (error
   (user-set-change
    ("user not in the set [using-locked-sessions]" (unix-login-for-testing))))
  [1]
  $ fe admin users using-locked-sessions add unix-login-for-testing

Check that admin privileges is required for a user to add someone else in this set,
but not required when they are adding themselves.

  $ IRON_USER=user1 fe admin users using-locked-sessions add unix-login-for-testing
  (error
   (user-set-change
    ("unauthorized RPC by user -- admin privileges required"
     ((user user1) (users_with_admin_privileges (unix-login-for-testing))))))
  [1]
  $ IRON_USER=user1 fe admin users using-locked-sessions add user1

Check warning about stale session.

  $ fe session show
  Warning: the feature has changed since this session was created.  It may be more suitable
  to review the feature to its most recent tip.  Consider committing your session:
  
  |-----------------------------------------------------------------|
  | remaining in session | session end to tip | remaining if commit |
  |----------------------+--------------------+---------------------|
  |                    5 |                  5 |                  10 |
  |-----------------------------------------------------------------|
  
  Reviewing test to 9bdd8ea4bdfa.
  1 files to review (1 already reviewed): 8 lines total
     [X] 3 a
     [ ] 5 b

  $ fe session commit -session-id ${ID}
  $ fe session show
  Reviewing test to 4ca86eeaa256.
  1 files to review: 10 lines total
     [ ] 10 b
