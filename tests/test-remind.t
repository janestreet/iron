Start test.

  $ start_test

Setup a repo with two files:

  $ rm -rf ./repo
  $ copy_sample_repo two-users ./repo
  $ cd repo
  $ hg init
  $ remote="$PWD"
  $ hg add -q .
  $ hg commit -m "init"
  $ hg bookmark test
  $ fe create -no-bookmark -tip . test -remote "$remote" -desc 'root for test'
  $ feature_to_server test
  $ fe enable-review test
  $ fe second -even-though-owner -even-though-empty test
  $ fe change test -set-who-can-release-into-me my-owners

Create the feature, with an owner and a whole-feature reviewer:

  $ fe create test/a -desc 'a' -owner owner
  $ (echo 1; echo 3) > a  # Hey, there's no 2 in this file.
  $ seq 1 5 > b
  $ hg commit -m "add some lines"
  $ feature_to_server test/a
  $ fe change test/a -add-whole-feature-reviewers seconder

Leave a CR about the missing 2 for user2:

  $ (echo 1; echo "# $CR user1 for user2: Where is the 2?"; echo 3) > a
  $ hg commit -m "added cr for user2"
  $ feature_to_server test/a

Right now, there's no reviewing on, so only user2 (who has a CR) needs email:

  $ echo n | fe remind test/a -interactive true
  Sending mail
  
  ------
  Subject: reminder for Iron feature test/a
  
  |---------------------|
  | user  | CRs | total |
  |-------+-----+-------|
  | user2 |   1 |     1 |
  | total |   1 |     1 |
  |---------------------|
  
  test/a
  ======
  a
  ------
  to the following users: 
    owner
    user2
  
  Send mail? [y/n/e/?]: Aborted

Now, enable reviewing.

  $ fe enable-review test/a

Let's have the seconder complete some review so we can check that the remind
email show completed lines as well as review sessions in progress.

  $ fe internal session mark-file test/a a -for seconder -reason test
  $ echo n | fe remind test/a -interactive true
  Sending mail
  
  ------
  Subject: reminder for Iron feature test/a
  
  |---------------------|
  | user  | CRs | total |
  |-------+-----+-------|
  | user2 |   1 |     1 |
  | total |   1 |     1 |
  |---------------------|
  
  |------------------------------------------|
  | user     | review | catch-up | completed |
  |----------+--------+----------+-----------|
  | owner    |      8 |          |           |
  | seconder |      5 |        3 |         3 |
  |------------------------------------------|
  
  |-----------------------------|
  | review sessions in progress |
  |-----------------------------|
  | seconder                    |
  |-----------------------------|
  
  test/a
  ======
  a
  ------
  to the following users: 
    owner
    seconder
    user2
  
  Send mail? [y/n/e/?]: Aborted

Let's change the reviewer set and observe that the line-count table entries change.

  $ SESSION_ID=$(fe session show -id test/a -for seconder)
  $ IRON_USER=seconder fe session forget test/a -session-id ${SESSION_ID} -file a
  $ IRON_USER=seconder fe session commit test/a -session-id ${SESSION_ID}
  $ fe change test/a -set-reviewing user2
  $ feature_to_server test/a
  $ echo n | fe remind test/a -interactive true
  Sending mail
  
  ------
  Subject: reminder for Iron feature test/a
  
  |---------------------|
  | user  | CRs | total |
  |-------+-----+-------|
  | user2 |   1 |     1 |
  | total |   1 |     1 |
  |---------------------|
  
  |----------------|
  | user  | review |
  |-------+--------|
  | user2 |      5 |
  |----------------|
  
  test/a
  ======
  a
  ------
  to the following users: 
    owner
    user2
  
  Send mail? [y/n/e/?]: Aborted

When the feature is ready to be seconded, send the reminder to w-f-reviewers and owners.

  $ (seq 1 3) > a
  $ hg commit -m "resolved cr"
  $ feature_to_server test/a

  $ fe change -set-reviewing-all
  $ IRON_USER=owner fe internal mark-fully-reviewed test/a
  $ fe internal mark-fully-reviewed test/a -for all -reason reason
  $ echo n | fe remind test/a -interactive true
  Sending mail
  
  ------
  Subject: reminder for Iron feature test/a
  
  test/a
  ======
  a
  ------
  to the following users: 
    owner
    seconder
  
  Send mail? [y/n/e/?]: Aborted

Second the feature, make it releasable.

  $ IRON_USER=seconder fe second
  $ fe show -next-steps
  (Release)

When the feature is releasable, send the reminder to the child owners and the
parent owners, since only them can do the release.

  $ fe show test   -owners
  (unix-login-for-testing)

  $ fe show test/a -owners
  (owner)

  $ fe remind test/a -just-print-recipients-and-exit
  owner
  unix-login-for-testing

Make sure the feature is in the parent's todo, in the 'Features you watch' table.

  $ fe todo
  Features you watch:
  |---------------------|
  | feature | next step |
  |---------+-----------|
  | test    |           |
  |   a     | release   |
  |---------------------|

  $ fe todo -for owner
  Features you own:
  |---------------------|
  | feature | next step |
  |---------+-----------|
  | test    |           |
  |   a     | release   |
  |---------------------|

If the child owner can do the release, no need to involve the parent owner.

  $ fe change test -set-who-can-release-into-me my-owners-and-child-owners
  $ fe remind test/a -just-print-recipients-and-exit
  owner

  $ fe todo
