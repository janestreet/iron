Start test.

  $ start_test

Setup repo.

  $ setup_repo_and_root .fe.sexp file
  $ mkdir .fe
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Define_scrutiny normal  ((level 50) (description "normal")))
  > (Define_scrutiny ignore  ((level  0) (description "ignore")))
  > (Users unix-login-for-testing user1 file-follower file-owner)
  > (Define_group file-followers (file-follower))
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
  >   (Apply_to (Files file)))
  > EOF
  $ hg add -q .
  $ hg commit -q -m 'added files'
  $ fe enable
  $ fe change -set-base root
  $ feature_to_server root
  $ fe ob check
  $ fe second -even-though-owner -even-though-empty
  $ ORIGIN=$(fe show -base root)

Create a child feature, adding a follower, and check who has review to do.

  $ fe create root/add-follower -description 'add a file follower'
  $ feature_to_server root/add-follower
  $ fe enable-review root/add-follower
  $ fe second -even-though-owner -even-though-empty
  $ cat >>.fe.sexp <<EOF
  > (Local
  >   (Followers (Group file-followers))
  >   (Apply_to (Files file)))
  > EOF
  $ hg commit -q -m 'added a file follower'
  $ feature_to_server root/add-follower

Check that only the changes to .fe.sexp are to be reviewed.  Note that there are no
attributes changes to be reviewed.  File owners and reviewers should not have to care
about who follow their files, or there would be too much friction in adding followers.

  $ fe show -omit-description -omit-attribute-table
  root/add-follower
  =================
  
  |------------------------------------------|
  | user                   | review | follow |
  |------------------------+--------+--------|
  | unix-login-for-testing |      3 |        |
  | file-follower          |        |      1 |
  |------------------------------------------|

  $ fe show -who-can-review
  file-follower
  unix-login-for-testing

  $ fe session diff -do-not-lock-session | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ .fe.sexp @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | tip * (glob)
  @@@@@@@@ base 1,11 tip 1,14 @@@@@@@@
    (Local
      (Owner unix-login-for-testing)
      (Scrutiny normal)
      (Reviewed_by (All_of (Users unix-login-for-testing)))
      (Apply_to (Files .fe.sexp)))
    (Local
      (Owner file-owner)
      (Scrutiny normal)
      (Reviewed_by (All_of (Users user1)))
      (Apply_to (Files file)))
  +|(Local
  +|  (Followers (Group file-followers))
  +|  (Apply_to (Files file)))

  $ fe tools mark-fully-reviewed root/add-follower
  $ fe show -next-steps root/add-follower
  (Release)

  $ IRON_USER=file-follower fe session diff -do-not-lock-session | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  base * | tip * (glob)
  @@@@@@@@ base 1,6 tip 1,6 @@@@@@@@
    file                      = file
    scrutiny                  = normal
    owner                     = file-owner
    reviewed by               = (All_of (Users user1))
  -|followed by file-follower = no
  +|followed by file-follower = yes

  $ IRON_USER=file-follower fe tools mark-fully-reviewed root/add-follower

  $ fe release
  $ feature_to_server root

Recreate a child to make more changes.

  $ fe create root/add-follower -description 'add a file follower'
  $ feature_to_server root/add-follower
  $ fe enable-review root/add-follower
  $ fe second -even-though-owner -even-though-empty

Check what happens when the file is deleted.

  $ BEFORE_DELETE=$(fe show -tip)
  $ cat >.fe.sexp <<EOF
  > (Owner unix-login-for-testing)
  > (Scrutiny normal)
  > (Reviewed_by (All_of (Users unix-login-for-testing)))
  > (Apply_to (Files .fe.sexp))
  > EOF
  $ hg rm file
  $ hg commit -m "delete the file"
  $ feature_to_server root/add-follower

  $ fe show -omit-description -omit-attribute-table
  root/add-follower
  =================
  
  |------------------------------------------|
  | user                   | review | follow |
  |------------------------+--------+--------|
  | unix-login-for-testing |     12 |        |
  | file-owner             |      2 |        |
  | user1                  |      2 |        |
  | file-follower          |        |      2 |
  |------------------------------------------|

  $ IRON_USER=file-follower fe session show
  Reviewing root/add-follower to *. (glob)
  1 files to review: 2 lines total
  
  Follow review.
  Your pending review on these changes does not prevent releasability.
  These files are shown to you just so you can follow along.
     [ ] 2 file

Verify that the red lines are shown for the deleted file.

  $ IRON_USER=file-follower fe session diff -do-not-lock-session | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  base file = file
  tip file  = <absent>
  base * | tip * (glob)
  _
  | @@@@@@@@ Hunk 1/2 @@@@@@@@
  | @@@@@@@@ base 1,6 tip 1,2 @@@@@@@@
  | -|file                      = file
  | -|scrutiny                  = normal
  | -|owner                     = file-owner
  | -|reviewed by               = (All_of (Users user1))
  | -|followed by file-follower = yes
  | +|<absent>
  |_
  _
  | @@@@@@@@ Hunk 2/2 @@@@@@@@
  | @@@@@@@@ base 1,2 tip 1,1 @@@@@@@@
  | -|file
  |_

Put the file back.

  $ hg cat -r ${BEFORE_DELETE} .fe.sexp >.fe.sexp
  $ hg cat -r ${BEFORE_DELETE} file >file
  $ hg add file; hg commit -m "put it back"
  $ feature_to_server root/add-follower

Now add some changes in the file, check what happens.

  $ echo "A change happens in the file" >file
  $ hg commit -q -m 'change'
  $ feature_to_server root/add-follower
  $ fe show -omit-description -omit-attribute-table
  root/add-follower
  =================
  
  |------------------------------------------|
  | user                   | review | follow |
  |------------------------+--------+--------|
  | unix-login-for-testing |      1 |        |
  | user1                  |      1 |        |
  | file-follower          |        |      1 |
  |------------------------------------------|

All normal users can review for themselves, or via catch-up as usual.

  $ fe tools mark-fully-reviewed root/add-follower
  $ fe tools mark-fully-reviewed root/add-follower -for user1 -reason 'reason'

But one cannot review for a file follower, it serves no purpose.

  $ fe tools mark-fully-reviewed root/add-follower -for file-follower -reason reason
  (error
   (may-modify-others-review
    ("unauthorized review for a user with only lines to follow" file-follower)))
  [1]

  $ IRON_USER=file-follower fe tools mark-fully-reviewed root/add-follower

  $ fe show -omit-description -omit-attribute-table
  root/add-follower
  =================
  
  |-----------------------------------------------|
  | user                   | catch-up | completed |
  |------------------------+----------+-----------|
  | user1                  |        1 |         1 |
  | file-follower          |          |         1 |
  | unix-login-for-testing |          |         1 |
  |-----------------------------------------------|

  $ fe show -next-steps root/add-follower
  (Release)
  $ IRON_USER=user1 fe catch-up clear root/add-follower

Now try to see what a file-follower sees when they are removed as file-follower
with some follow review already done.

  $ CHECK_POINT=$(fe show -tip)
  $ echo "Dropped from file followers with a brain" >file
  $ hg commit -q -m 'change'
  $ feature_to_server root/add-follower
  $ IRON_USER=file-follower fe session diff -do-not-lock-session | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | old tip * | new tip * (glob)
  @@@@@@@@ old tip 1,2 new tip 1,2 @@@@@@@@
  -|A change happens in the file
  +|Dropped from file followers with a brain

  $ IRON_USER=file-follower fe tools mark-fully-reviewed root/add-follower
  $ hg cat -r ${ORIGIN} .fe.sexp >.fe.sexp
  $ hg commit -q -m 'temporarily removed a file follower'
  $ feature_to_server root/add-follower
  $ fe show -omit-description -omit-attribute-table
  root/add-follower
  =================
  
  |------------------------------------------------------|
  | user                   | review | follow | completed |
  |------------------------+--------+--------+-----------|
  | unix-login-for-testing |      5 |        |         1 |
  | user1                  |      2 |        |         1 |
  | file-follower          |        |      1 |         1 |
  |------------------------------------------------------|

  $ IRON_USER=file-follower fe session diff -do-not-lock-session | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  base * | old tip * | new tip * (glob)
  @@@@@@@@ old tip, base 1,6 new tip 1,6 @@@@@@@@
    file                      = file
    scrutiny                  = normal
    owner                     = file-owner
    reviewed by               = (All_of (Users user1))
  -|followed by file-follower = yes
  +|followed by file-follower = no

Add him back and go back to previous state as of CHECK_POINT.

  $ hg cat -r ${CHECK_POINT} file >file
  $ hg cat -r ${CHECK_POINT} .fe.sexp >.fe.sexp
  $ hg commit -q -m 'put him back'
  $ feature_to_server root/add-follower
  $ IRON_USER=file-follower fe tools mark-fully-reviewed root/add-follower
  $ fe show -next-steps
  (Release)

This time, add a change but release before the file-follower reviews it.

  $ echo "Another change happens in the file" >file
  $ hg commit -q -m 'change'
  $ feature_to_server root/add-follower
  $ fe show -omit-description -omit-attribute-table
  root/add-follower
  =================
  
  |------------------------------------------------------|
  | user                   | review | follow | completed |
  |------------------------+--------+--------+-----------|
  | unix-login-for-testing |      2 |        |         1 |
  | user1                  |      2 |        |         1 |
  | file-follower          |        |      2 |         1 |
  |------------------------------------------------------|

  $ fe tools mark-fully-reviewed root/add-follower
  $ IRON_USER=user1 fe tools mark-fully-reviewed root/add-follower

  $ fe show -next-steps root/add-follower
  (Release)
  $ fe release root/add-follower
  $ feature_to_server root

  $ IRON_USER=file-follower fe todo
  |---------------------------|
  | feature        | catch-up |
  |----------------+----------|
  | root           |          |
  |   add-follower |        2 |
  |---------------------------|

  $ IRON_USER=file-follower fe catch-up diff root/add-follower | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | old tip * | new tip * (glob)
  @@@@@@@@ old tip 1,2 new tip 1,2 @@@@@@@@
  -|A change happens in the file
  +|Another change happens in the file

Iron retains the reason the catch-up was generated, and show it to the user.

  $ IRON_USER=file-follower fe catch-up show root/add-follower \
  >  |& matches "The feature was released and you were a follower."

  $ IRON_USER=file-follower fe catch-up clear root/add-follower

Now create a new child removing the file follower, and check what happens.

  $ fe create root/remove-follower -description 'remove a file follower'
  $ feature_to_server root/remove-follower
  $ fe enable-review root/remove-follower
  $ fe second -even-though-owner -even-though-empty
  $ hg cat -r ${ORIGIN} .fe.sexp >.fe.sexp
  $ hg commit -q -m 'removed a file follower'
  $ feature_to_server root/remove-follower

Check that when removing a follower, there is nothing to review other than the
change to the file [.fe.sexp], expect for the follower themselves.

  $ fe show -omit-description -omit-attribute-table
  root/remove-follower
  ====================
  
  |------------------------------------------|
  | user                   | review | follow |
  |------------------------+--------+--------|
  | unix-login-for-testing |      3 |        |
  | file-follower          |        |      1 |
  |------------------------------------------|

  $ IRON_USER=file-follower fe session diff -do-not-lock-session | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  base * | tip * (glob)
  @@@@@@@@ base 1,6 tip 1,6 @@@@@@@@
    file                      = file
    scrutiny                  = normal
    owner                     = file-owner
    reviewed by               = (All_of (Users user1))
  -|followed by file-follower = yes
  +|followed by file-follower = no

  $ IRON_USER=file-follower fe tools mark-fully-reviewed root/remove-follower

  $ fe session diff -do-not-lock-session | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ .fe.sexp @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | tip * (glob)
  @@@@@@@@ base 1,14 tip 1,11 @@@@@@@@
    (Local
      (Owner unix-login-for-testing)
      (Scrutiny normal)
      (Reviewed_by (All_of (Users unix-login-for-testing)))
      (Apply_to (Files .fe.sexp)))
    (Local
      (Owner file-owner)
      (Scrutiny normal)
      (Reviewed_by (All_of (Users user1)))
      (Apply_to (Files file)))
  -|(Local
  -|  (Followers (Group file-followers))
  -|  (Apply_to (Files file)))

  $ fe tools mark-fully-reviewed root/remove-follower
  $ fe show -next-steps root/remove-follower
  (Release)

Check that subsequent changes to the file are no longer followed by the follower.

  $ echo "After the follower is dropped, some changes happen in the file" >file
  $ hg commit -q -m 'change'
  $ feature_to_server root/remove-follower
  $ fe show -omit-description -omit-attribute-table
  root/remove-follower
  ====================
  
  |---------------------------------------------|
  | user                   | review | completed |
  |------------------------+--------+-----------|
  | unix-login-for-testing |      2 |         3 |
  | user1                  |      2 |           |
  | file-follower          |        |         1 |
  |---------------------------------------------|
