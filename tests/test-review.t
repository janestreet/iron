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
  $ hg bookmark test

It is an error to review nonexistent features.

  $ fe review test
  ("no such feature" test)
  [1]

Create the feature

  $ fe create -no-bookmark -tip . test -remote "$remote" -desc 'root for test'
  $ BOOKMARK=test fe internal hydra ; hg -q update -r test

It is an error to review a feature when not enabled

  $ fe review test |& matches "not enabled"
  [1]

Can enable without flag if we also add whole feature reviewers

  $ fe enable-review test -add-whole-feature-reviewers jdoe3

We drop non started review sessions when the tip moves.

  $ review_goal=$(fe session show -tip)
  $ hg log -r . --template='{node}\n' | matches ${review_goal}
  $ seq 4 6 > a
  $ hg commit -m 2
  $ feature_to_server test
  $ review_goal2=$(fe session show -tip)
  $ hg log -r . --template='{node}\n' | matches $review_goal2
  $ test ${review_goal} = ${review_goal2}
  [1]
  $ review_goal=${review_goal2}

Review

  $ fe internal session show-num-lines test
  8
  $ fe session mark-file test a
  $ fe internal session show-num-lines test
  5

  $ fe session show -tip | matches $review_goal
  $ seq 1 3 > a
  $ hg commit -m 3
  $ feature_to_server test
  $ fe session show -tip | matches $review_goal

Persistence

  $ fe-server stop
  $ fe-server start

  $ fe internal session show-num-lines test
  5

  $ fe session mark-file test a |& matches "already reviewed"
  [1]

  $ fe session mark-file test b

A new session is now created since [b] was the last file to review in
the former session.

  $ fe internal session show-num-lines test
  6

  $ fe session mark-file test a
  $ fe internal session show-num-lines test
  0

Not enabled for other users

  $ fe review test -for user1 -reason reason |& matches "user is not reviewing"
  [1]

  $ fe review test -for user2 -reason reason |& matches "user is not reviewing"
  [1]

Enable more users to review

  $ fe change test -set-reviewing 'user1,user2'

  $ fe internal session show-num-lines test -for user1
  3

  $ fe internal session show-num-lines test -for user2
  5

Change whole feature reviewer

  $ fe change -add-whole-feature-reviewers user1
  $ fe internal session show-num-lines test -for user1
  8

  $ fe change -remove-whole-feature-reviewers user1
  $ fe internal session show-num-lines test -for user1
  3

  $ fe show -reviewers
  jdoe3
  user2
  user1
  unix-login-for-testing

  $ fe brain show -for all
  jdoe3:
  
  unix-login-for-testing:
  |--------------------|
  | file | op  | lines |
  |------+-----+-------|
  | a    | mod |     3 |
  | b    | mod |     5 |
  |--------------------|
  
  user1:
  
  user2:
  
  $ fe session show -for all
  jdoe3:
  Reviewing test to 07c38bb53172.
  2 files to review: 8 lines total
     [ ] 3 a
     [ ] 5 b
  
  unix-login-for-testing:
  ("reviewer is up to date, no current session" (test unix-login-for-testing))
  
  user1:
  Reviewing test to 07c38bb53172.
  1 files to review: 3 lines total
     [ ] 3 a
  
  user2:
  Reviewing test to 07c38bb53172.
  1 files to review: 5 lines total
     [ ] 5 b
  
Test [-create-catch-up-for-me].

  $ fe todo -crs-and-review
  $ echo y | fe brain forget -all >/dev/null
  $ fe change -add-reviewing unix-login-for-testing
  $ fe todo -crs-and-review
  |------------------|
  | feature | review |
  |---------+--------|
  | test    |      8 |
  |------------------|
  $ fe session mark-file test a -create-catch-up-for-me
  $ fe todo -crs-and-review
  |-----------------------------|
  | feature | review | catch-up |
  |---------+--------+----------|
  | test    |      5 |        3 |
  |-----------------------------|

Test that no exceptions are raised in the background if no user request to see
diffs in the interactive mode of fe review.

  $ export IRON_OPTIONS='((display_ascii_always false))'
  $ export IRON_FUNCTIONAL_TESTING_CLIENT_RAISES_CREATING_HUNKS=1
  $ echo "q" | fe review test | matches "Reviewing test.*Quit"
