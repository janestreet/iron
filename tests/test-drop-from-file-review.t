Start test.

  $ start_test

  $ hg init repo
  $ cd repo
  $ remote=$PWD

Make a feature with a file changes from bi to fi where [user1]
reviews at bi but not at fi.

  $ touch file; hg add file

  $ echo 'base'>file; hg com -m old-base
  $ rev_b1=$(hg log -r . --template {node})

  $ echo 'tip'>file; hg com -m old-tip
  $ rev_f1=$(hg log -r . --template {node})

  $ echo 'base'>file; hg com -m new-base
  $ rev_b2=$(hg log -r . --template {node})

  $ echo 'tip'>file; hg com -m new-tip
  $ rev_f2=$(hg log -r . --template {node})

  $ attribute=$(cat <<EOF
  > ($rev_b1 ((file_reviewers (user1))))
  > ($rev_f1 ((file_reviewers ())))
  > ($rev_b2 ((file_reviewers (user2))))
  > ($rev_f2 ((file_reviewers ())))
  > EOF
  > )

  $ hg update -q -r ${rev_f1}
  $ fe create root -remote $remote -description root -base ${rev_b1} -tip ${rev_f1}
  $ feature_to_server root -fake-valid -fake-attribute "$attribute"

Enable review for [user1], and they have one line to review -- the attribute change.

  $ fe enable-review
  $ fe change -set-reviewing user1
  $ fe todo -for user1
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |      1 |
  |------------------|
  $ fe session diff -for user1 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  base * | tip * (glob)
  @@@@@@@@ base 1,5 tip 1,5 @@@@@@@@
    file        = file
    scrutiny    = level10
    owner       = file-owner
  -|reviewed by = (All_of (Users user1))
  +|reviewed by = None

But if [user1] is a whole-feature reviewer, they have four lines to review, because they
have to review the change.

  $ fe change -add-whole-feature-reviewer user1
  $ fe todo -for user1
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |      3 |
  |------------------|
  $ fe session diff -for user1 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  base * | tip * (glob)
  _
  | @@@@@@@@ Hunk 1/2 @@@@@@@@
  | @@@@@@@@ base 1,5 tip 1,5 @@@@@@@@
  |   file        = file
  |   scrutiny    = level10
  |   owner       = file-owner
  | -|reviewed by = (All_of (Users user1))
  | +|reviewed by = None
  |_
  _
  | @@@@@@@@ Hunk 2/2 @@@@@@@@
  | @@@@@@@@ base 1,2 tip 1,2 @@@@@@@@
  | -|base
  | +|tip
  |_


  $ fe change -remove-whole-feature-reviewers user1
  $ IRON_USER=user1 fe tools mark-fully-reviewed root
  $ fe tools mark-fully-reviewed root -for all -reason reason

  $ fe change -set-base ${rev_b2}
  $ hg book -f -r ${rev_f2} root
  $ feature_to_server root -fake-valid -fake-attribute "$attribute"

  $ fe session diff -for user1 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny level10
  old base * | old tip * (glob)
  @@@@@@@@ Forget this diff -- this file no longer has a diff you should know @@@@@@@@
  @@@@@@@@ old base 1,2 old tip 1,2 @@@@@@@@
  -|base
  +|tip

  $ fe session diff | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  old base * | old tip * | new base * | new tip * (glob)
  @@@@@@@@ View : feature-ddiff @@@@@@@@
  @@@@@@@@ A base change was dropped in favor of a feature change @@@@@@@@
  @@@@@@@@ -- old base 1,6 new tip, old tip 1,6 @@@@@@@@
  @@@@@@@@ ++ new base 1,6 old tip, new tip 1,6 @@@@@@@@
      file        = file
      scrutiny    = level10
      owner       = file-owner
  ---|reviewed by = (All_of (Users user1))
  ++-|reviewed by = (All_of (Users user2))
    +|reviewed by = None

Demonstrate that once one has reviewed the fact that one is dropped as
a file reviewer, then one doesn't have to review that again after a
rebase.

  $ attribute=$(cat <<EOF
  > ($rev_b1 ((file_reviewers (user1))))
  > ($rev_f1 ((file_reviewers (user1))))
  > ($rev_b2 ((file_reviewers ())))
  > ($rev_f2 ((file_reviewers ())))
  > EOF
  > )

  $ export IRON_USER=user1
  $ fe change -set-base ${rev_b1}
  $ hg book -f -r ${rev_b2} root
  $ feature_to_server root -fake-valid -fake-attribute "$attribute"
  $ fe brain forget -all
  $ fe session diff | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  base ce8d6e8cf6e0 | tip 1d8e9b268ccd
  @@@@@@@@ base 1,5 tip 1,5 @@@@@@@@
    file        = file
    scrutiny    = level10
    owner       = file-owner
  -|reviewed by = (All_of (Users user1))
  +|reviewed by = None

  $ fe tools mark-fully-reviewed root

  $ fe change -set-base ${rev_f1}
  $ hg book -f -r ${rev_f2} root
  $ feature_to_server root -fake-valid -fake-attribute "$attribute"
  $ fe session diff | fe internal remove-color
  ("reviewer is up to date, no current session" (root user1))
  [1]
