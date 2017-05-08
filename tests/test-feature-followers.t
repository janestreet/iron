Start test.

  $ start_test

Setup repo.

  $ setup_repo_and_root .fe.sexp file file2
  $ mkdir .fe
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Define_scrutiny normal  ((level 50) (description "normal")))
  > (Define_scrutiny ignore  ((level  0) (description "ignore")))
  > (Users unix-login-for-testing user1 user2 file-owner)
  > EOF
  $ touch .fe/obligations-repo.sexp
  $ cat >.fe/.fe.sexp <<EOF
  > (Owner unix-login-for-testing)
  > (Scrutiny ignore)
  > (Apply_to All_files)
  > EOF
  $ cat >.fe.sexp <<EOF
  > (Local
  >   (Owner unix-login-for-testing)
  >   (Scrutiny normal)
  >   (Reviewed_by (All_of (Users unix-login-for-testing)))
  >   (Apply_to (Files .fe.sexp)))
  > (Local
  >   (Owner file-owner)
  >   (Scrutiny normal)
  >   (Reviewed_by (All_of (Users user1)))
  >   (Apply_to (Files file file2)))
  > EOF
  $ hg add -q .
  $ hg commit -q -m 'added files'
  $ fe enable
  $ fe change -set-base root
  $ feature_to_server root
  $ fe ob check
  $ fe second -even-though-owner -even-though-empty
  $ ORIGIN=$(fe show -base root)

Create a child feature, add a w-f-follower.

  $ fe create root/add-follower -description 'hello description'
  $ feature_to_server root/add-follower
  $ fe enable-review root/add-follower
  $ fe change -add-whole-feature-follower user2 root/add-follower

Check that the whole feature follower sees the feature in their todo.

  $ fe todo -for user2
  Features you watch:
  |----------------------------|
  | feature        | next step |
  |----------------+-----------|
  | root           |           |
  |   add-follower | add code  |
  |----------------------------|

But if they own it as well, there is no need to show it in the watch table in
addition to the owned table.

  $ fe change root/add-follower -add-owners user2
  $ fe todo -for user2
  Features you own:
  |----------------------------|
  | feature        | next step |
  |----------------+-----------|
  | root           |           |
  |   add-follower | add code  |
  |----------------------------|
  $ fe change root/add-follower -remove-owners user2

Also, if the feature is 'not interesting', it is temporarily hidden.
Example, if it is an umbrella feature with no code in it.

  $ fe change -set-is-permanent true root/add-follower
  $ fe todo -for user2
  $ fe change -set-is-permanent false root/add-follower

Check what a whole-feature follower sees when a file is deleted in the feature.

  $ BEFORE_DELETE=$(fe show -tip)
  $ cat >.fe.sexp <<EOF
  > (Owner unix-login-for-testing)
  > (Scrutiny normal)
  > (Reviewed_by (All_of (Users unix-login-for-testing)))
  > (Apply_to (Files .fe.sexp))
  > EOF
  $ hg rm file file2
  $ hg commit -m "delete the files"
  $ feature_to_server root/add-follower

  $ fe show -omit-description -omit-attribute-table
  root/add-follower
  =================
  
  |------------------------------------------|
  | user                   | review | follow |
  |------------------------+--------+--------|
  | unix-login-for-testing |     11 |        |
  | file-owner             |      4 |        |
  | user1                  |      4 |        |
  | user2                  |        |     11 |
  |------------------------------------------|

If there is only one w-f-reviewer, having them in the reviewing set is not enough to
implicitly enable the w-f-followers.  This allows owners to review their changes on their
own first.

  $ fe change -set-reviewing-whole-feature-only

  $ fe show -whole-feature-reviewers
  (unix-login-for-testing)

  $ fe show -reviewing
  "whole-feature reviewers"

  $ fe show -who-can-review
  unix-login-for-testing

However, when more w-f-reviewers are enabled, the w-f-followers are enabled too, even
prior to seconding.  The feature is starting to see some collobarative work being done,
this may interest the w-f-followers.

  $ fe change -add-whole-feature-reviewers file-owner
  $ fe change -set-reviewing-whole-feature-only

  $ fe show -who-can-review
  file-owner
  unix-login-for-testing
  user2

Check that adding another reviewing user does not accidentally disable the followers.

  $ fe change -add-reviewing user1
  $ fe show -who-can-review
  file-owner
  unix-login-for-testing
  user1
  user2

Another case is when the feature is seconded, even when there is only 1 w-f-reviewer.

  $ fe change -remove-whole-feature-reviewers file-owner
  $ fe second -even-though-owner
  $ fe change -set-reviewing-whole-feature-only

  $ fe show -who-can-review
  unix-login-for-testing
  user2

When all are enabled, this obviously includes the w-f-followers as well.

  $ fe change -set-reviewing-all

  $ fe show -reviewing
  all

  $ fe show -who-can-review
  file-owner
  unix-login-for-testing
  user1
  user2

  $ IRON_USER=user2 fe session show
  Reviewing root/add-follower to a38efa6fe814.
  3 files to review: 11 lines total
  
  Follow review.
  Your pending review on these changes does not prevent releasability.
  These files are shown to you just so you can follow along.
     [ ] 7 .fe.sexp
     [ ] 2 file
     [ ] 2 file2

Verify that the red lines are shown for the deleted file.

  $ IRON_USER=user2 fe session diff -file file -do-not-lock-session | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  base file = file
  tip file  = <absent>
  base * | tip * (glob)
  _
  | @@@@@@@@ Hunk 1/2 @@@@@@@@
  | @@@@@@@@ base 1,5 tip 1,2 @@@@@@@@
  | -|file        = file
  | -|scrutiny    = normal
  | -|owner       = file-owner
  | -|reviewed by = (All_of (Users user1))
  | +|<absent>
  |_
  _
  | @@@@@@@@ Hunk 2/2 @@@@@@@@
  | @@@@@@@@ base 1,2 tip 1,1 @@@@@@@@
  | -|file
  |_

And there should be no diff between what a w-f-follower and w-f-reviewer see.

  $ diff \
  >   <(fe session diff            -do-not-lock-session | fe internal remove-color) \
  >   <(fe session diff -for user2 -do-not-lock-session | fe internal remove-color)

Put the files back.

  $ hg revert -r ${BEFORE_DELETE} --all -q
  $ hg commit -m "put the files back"
  $ feature_to_server root/add-follower

Add some changes in the file, check what happens.

  $ echo "A change happens" >file
  $ echo "A change happens" >file2
  $ hg commit -q -m 'change'
  $ feature_to_server root/add-follower
  $ fe show -omit-description -omit-attribute-table
  root/add-follower
  =================
  
  |------------------------------------------|
  | user                   | review | follow |
  |------------------------+--------+--------|
  | unix-login-for-testing |      4 |        |
  | user1                  |      4 |        |
  | user2                  |        |      4 |
  |------------------------------------------|

The feature followers have the feature they follow in their watch table.

  $ fe todo -for user2
  |-------------------------|
  | feature        | follow |
  |----------------+--------|
  | root           |        |
  |   add-follower |      4 |
  |-------------------------|
  
  Features you watch:
  |------------------------------------|
  | feature        | #left | next step |
  |----------------+-------+-----------|
  | root           |       |           |
  |   add-follower |     2 | review    |
  |------------------------------------|

  $ IRON_USER=user2 fe todo -watched-by-me-names
  root/add-follower

All normal users can review for them self, or via catch-up as usual.

  $ fe tools mark-fully-reviewed root/add-follower
  $ IRON_USER=user1 fe tools mark-fully-reviewed root/add-follower

  $ fe show -next-steps root/add-follower
  (Release)

But one cannot review for a feature follower, it serves no purpose.

  $ fe tools mark-fully-reviewed root/add-follower -for user2 -reason reason
  (error
   (may-modify-others-review
    ("unauthorized review for a user with only lines to follow" user2)))
  [1]

Check that a partial session is not blocking release if this is for a user who
only has follow lines.

  $ IRON_USER=user2 fe session mark-file root/add-follower file
  $ fe show -next-steps root/add-follower
  (Release)

  $ fe show -omit-description -omit-attribute-table
  root/add-follower
  =================
  
  |---------------------------------------------|
  | user                   | follow | completed |
  |------------------------+--------+-----------|
  | user2                  |      2 |         2 |
  | unix-login-for-testing |        |         4 |
  | user1                  |        |         4 |
  |---------------------------------------------|
  
  |-----------------------------|
  | review sessions in progress |
  |-----------------------------|
  | user2                       |
  |-----------------------------|

  $ IRON_USER=user2 fe tools mark-fully-reviewed root/add-follower
  $ fe show -next-steps root/add-follower
  (Release)

Now try to see what a w-f-follower sees when they are removed as w-f-follower
with some follow review already done.

  $ CHECK_POINT=$(fe show -tip)
  $ echo "Dropped from followers with a brain" >file
  $ hg commit -q -m 'change'
  $ feature_to_server root/add-follower
  $ IRON_USER=user2 fe session diff -do-not-lock-session | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | old tip * | new tip * (glob)
  @@@@@@@@ old tip 1,2 new tip 1,2 @@@@@@@@
  -|A change happens
  +|Dropped from followers with a brain

  $ IRON_USER=user2 fe tools mark-fully-reviewed root/add-follower
  $ fe change -remove-whole-feature-follower user2
  $ fe show -omit-description -omit-attribute-table
  root/add-follower
  =================
  
  |------------------------------------------------------|
  | user                   | review | follow | completed |
  |------------------------+--------+--------+-----------|
  | unix-login-for-testing |      2 |        |         4 |
  | user1                  |      2 |        |         4 |
  | user2                  |        |      6 |           |
  |------------------------------------------------------|

  $ IRON_USER=user2 fe session diff -do-not-lock-session | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  old base * | old tip * (glob)
  _
  | @@@@@@@@ Hunk 1/2 @@@@@@@@
  | @@@@@@@@ Forget this diff -- this file no longer has a diff you should know @@@@@@@@
  | @@@@@@@@ old base 1,2 old tip 1,2 @@@@@@@@
  | -|file
  | +|Dropped from followers with a brain
  |_
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file2 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  _
  | @@@@@@@@ Hunk 2/2 @@@@@@@@
  | @@@@@@@@ Forget this diff -- this file no longer has a diff you should know @@@@@@@@
  | @@@@@@@@ old base 1,2 old tip 1,2 @@@@@@@@
  | -|file2
  | +|A change happens
  |_

  $ IRON_USER=user2 fe tools mark-fully-reviewed root/add-follower

Check that subsequent changes to the feature are no longer followed by user2.

  $ echo "After the follower is dropped, some changes happen in the file" >file
  $ hg commit -q -m 'change'
  $ feature_to_server root/add-follower
  $ fe show -omit-description -omit-attribute-table
  root/add-follower
  =================
  
  |---------------------------------------------|
  | user                   | review | completed |
  |------------------------+--------+-----------|
  | unix-login-for-testing |      2 |         4 |
  | user1                  |      2 |         4 |
  |---------------------------------------------|

Add him back and go back to previous state as of CHECK_POINT.

  $ hg cat -r ${CHECK_POINT} file >file
  $ fe change -add-whole-feature-follower user2
  $ hg commit -q -m 'put him back'
  $ feature_to_server root/add-follower
  $ IRON_USER=user2 fe tools mark-fully-reviewed root/add-follower
  $ fe show -next-steps
  (Release)

This time, add a change but release before the user2 reviews it.

  $ echo "Another change happens in the file" >file
  $ hg commit -q -m 'change'
  $ feature_to_server root/add-follower
  $ fe show -omit-description -omit-attribute-table
  root/add-follower
  =================
  
  |------------------------------------------------------|
  | user                   | review | follow | completed |
  |------------------------+--------+--------+-----------|
  | unix-login-for-testing |      2 |        |         4 |
  | user1                  |      2 |        |         4 |
  | user2                  |        |      2 |         4 |
  |------------------------------------------------------|

  $ fe tools mark-fully-reviewed root/add-follower
  $ IRON_USER=user1 fe tools mark-fully-reviewed root/add-follower

  $ fe show -next-steps root/add-follower
  (Release)
  $ fe release root/add-follower
  $ feature_to_server root

  $ IRON_USER=user2 fe todo
  |---------------------------|
  | feature        | catch-up |
  |----------------+----------|
  | root           |          |
  |   add-follower |        2 |
  |---------------------------|

  $ IRON_USER=user2 fe catch-up diff root/add-follower | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | old tip * | new tip * (glob)
  @@@@@@@@ old tip 1,2 new tip 1,2 @@@@@@@@
  -|A change happens
  +|Another change happens in the file

Iron retains the reason the catch-up was generated, and show it to the user.

  $ IRON_USER=user2 fe catch-up show root/add-follower \
  >  |& matches "The feature was released and you were a follower."

  $ IRON_USER=user2 fe catch-up clear root/add-follower
