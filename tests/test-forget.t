Start test.

  $ start_test

Create hg repo.

  $ copy_sample_repo two-users ./repo
  $ cd repo
  $ hg init
  $ hg add -q .
  $ hg com -m init
  $ remote=$(pwd)
  $ fe create root -owner owner -description root -remote-repo-path $remote
  $ cat >b <<EOF
  > line1
  > line2
  > line3
  > EOF
  $ echo "space space" >a
  $ hg com -m change
  $ feature_to_server root
  $ fe enable-review
  $ fe change -set-reviewing all
  $ IRON_USER=owner fe tools mark-fully-reviewed root
  $ IRON_USER=owner fe second -even-though-owner
  $ IRON_USER=user2 fe tools mark-fully-reviewed root

Make user1 a whole-feature reviewer and do his review.

  $ fe change -add-whole-feature-reviewer user1
  $ fe session show -for user1
  Reviewing root to *. (glob)
  2 files to review: 4 lines total
     [ ] 1 a
     [ ] 3 b
  $ IRON_USER=user1 fe tools mark-fully-reviewed root
  $ fe session show -for user1 |& matches "reviewer is up to date"
  [1]

Make user1 not a whole-feature reviewer and he has to forget his knowledge.

  $ fe change -remove-whole-feature-reviewer user1
  $ fe show -omit-description -omit-attribute-table
  root
  ====
  
  |----------------------------|
  | user  | follow | completed |
  |-------+--------+-----------|
  | user1 |      4 |         1 |
  | owner |        |         4 |
  | user2 |        |         3 |
  |----------------------------|

  $ fe session show -for user1
  Reviewing root to *. (glob)
  1 files to review: 4 lines total
  
  Follow review.
  Your pending review on these changes does not prevent releasability.
  These files are shown to you just so you can follow along.
     [ ] 4 b
  $ fe session diff -for user1 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ b @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  old base * | old tip * (glob)
  @@@@@@@@ Forget this diff -- this file no longer has a diff you should know @@@@@@@@
  @@@@@@@@ old base 1,1 old tip 1,4 @@@@@@@@
  +|line1
  +|line2
  +|line3

Check that implicitly reviewed diff are automatically committed.

  $ cat >a <<EOF
  > space  space
  > EOF
  $ hg com -m "zero line change"
  $ feature_to_server root

  $ fe show -next-steps
  (Release)

This follow review does not prevent release.

  $ IRON_USER=owner fe release

  $ fe todo -for user1
  |--------------------|
  | feature | catch-up |
  |---------+----------|
  | root    |        4 |
  |--------------------|

  $ fe catch-up show root -for user1 -omit-header -omit-attribute
  Reviewing root to *. (glob)
  1 files to review: 4 lines total
  
  Catch-up.  The feature was released and you had partial review done on these files.
     [ ] 4 b
