Start test.

  $ start_test

Create hg repo.

  $ mkdir repo
  $ cd repo
  $ hg init
  $ touch file
  $ hg add file
  $ hg com -m file
  $ remote=$(pwd)
  $ fe create root -owner owner -description root -remote-repo-path $remote
  $ cat >file <<EOF
  > line1
  > line2
  > line3
  > EOF
  $ hg com -m change
  $ feature_to_server root -fake-valid-obligations
  $ fe enable-review
  $ fe change -set-reviewing all

Make user1 a whole-feature reviewer and do his review.

  $ fe change -add-whole-feature-reviewer user1
  $ fe session show -for user1
  Reviewing root to 57023eb9094d.
  1 files to review: 3 lines total
     [ ] 3 file
  $ fe session mark-file root file -for user1 -reason reason
  $ fe session show -for user1 |& matches "reviewer is up to date"
  [1]

Delete the file and make sure user1 will see the deleted lines in red.

  $ hg rm file
  $ hg commit -m "delete"
  $ feature_to_server root -fake-valid-obligations
  $ fe session show -for user1
  Reviewing root to 6381c8f38de3.
  1 files to review: 4 lines total
     [ ] 4 file
  $ fe session diff -for user1 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  base file = file
  tip file  = <absent>
  base 04da3968e088 | old tip 57023eb9094d | new tip 6381c8f38de3
  _
  | @@@@@@@@ Hunk 1/2 @@@@@@@@
  | @@@@@@@@ old tip, base 1,5 new tip 1,2 @@@@@@@@
  | -|file        = file
  | -|scrutiny    = level10
  | -|owner       = file-owner
  | -|reviewed by = None
  | +|<absent>
  |_
  _
  | @@@@@@@@ Hunk 2/2 @@@@@@@@
  | @@@@@@@@ A change in the feature was reverted @@@@@@@@
  | @@@@@@@@ old tip 1,4 base, new tip 1,1 @@@@@@@@
  | -|line1
  | -|line2
  | -|line3
  |_
