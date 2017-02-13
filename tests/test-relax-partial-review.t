Start test.

  $ start_test

Setup repo.

  $ setup_repo_and_root .fe.sexp file other-file
  $ mkdir .fe
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Define_scrutiny normal  ((level 50) (description "normal")))
  > (Define_scrutiny ignore  ((level  0) (description "ignore")))
  > (Users unix-login-for-testing user1 user2)
  > EOF
  $ touch .fe/obligations-repo.sexp
  $ cat >.fe/.fe.sexp <<EOF
  > (Owner unix-login-for-testing)
  > (Scrutiny ignore)
  > (Apply_to All_files)
  > EOF
  $ cat >.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Reviewed_by (At_least 1 of_ (Users user1 user2)))
  > (Apply_to All_files)
  > EOF
  $ hg add -q .
  $ hg com -m 'added files'
  $ fe enable
  $ fe change -set-base root
  $ feature_to_server root
  $ fe ob check
  $ fe second -even-though-owner -even-though-empty

Create child.

  $ fe create root/child -d child
  $ echo first-change >file; echo change >other-file; hg com -m file
  $ feature_to_server root/child
  $ fe show -omit-attribute-table -omit-description
  root/child
  ==========
  
  |---------------------------------|
  | user                   | review |
  |------------------------+--------|
  | unix-login-for-testing |      4 |
  | user1                  |      4 |
  | user2                  |      4 |
  |---------------------------------|
  $ fe enable
  $ fe second -even-though-owner

Have user2 do a partial review.

  $ IRON_USER=user2 fe session mark-file root/child file

  $ echo second-change >file; hg com -m file
  $ feature_to_server root/child
  $ fe tools mark-fully-reviewed root/child

At this point user1 or user2 may review the feature.

  $ fe show -omit-attribute-table -omit-description
  root/child
  ==========
  
  |---------------------------------------------|
  | user                   | review | completed |
  |------------------------+--------+-----------|
  | user2                  |      4 |         2 |
  | user1                  |      4 |           |
  | unix-login-for-testing |        |         4 |
  |---------------------------------------------|
  
  |-----------------------------|
  | review sessions in progress |
  |-----------------------------|
  | user2                       |
  |-----------------------------|

  $ fe todo -for user2
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |        |
  |   child |      4 |
  |------------------|

While we are in that state, check for the regression of a bug that used to cause
review_kind to move while the session is under review.  When a reviewer is the
last one before review obligations are met, the kind used to move to optional in
that very user's session, which was confusing.

  $ IRON_USER=user1 fe session show root/child
  Reviewing root/child to e1dfb03d82b0.
  2 files to review: 4 lines total
     [ ] 2 file
     [ ] 2 other-file

  $ IRON_USER=user1 fe session mark-file root/child file

  $ IRON_USER=user1 fe session show root/child
  Reviewing root/child to e1dfb03d82b0.
  1 files to review (1 already reviewed): 4 lines total
     [X] 2 file
     [ ] 2 other-file

If user1 review the entire feature, user2 is no longer required to finish their
partial review.  Instead what is left to review becomes a follow review.

  $ IRON_USER=user1 fe tools mark-fully-reviewed root/child

  $ fe show -omit-attribute-table -omit-description
  root/child
  ==========
  
  |--------------------------------------------------------------|
  | user                   |         review | follow | completed |
  |------------------------+----------------+--------+-----------|
  | user2                  | commit session |      2 |         2 |
  | unix-login-for-testing |                |        |         4 |
  | user1                  |                |        |         4 |
  |--------------------------------------------------------------|
  
  |-----------------------------|
  | review sessions in progress |
  |-----------------------------|
  | user2                       |
  |-----------------------------|

  $ fe todo -for user2
  |-----------------------------------|
  | feature |         review | follow |
  |---------+----------------+--------|
  | root    |                |        |
  |   child | commit session |      2 |
  |-----------------------------------|

User2 has still the same current session.

  $ fe session show -for user2
  Reviewing root/child to dc0eb4634592.
  1 files to review (1 already reviewed): 4 lines total
  
  Required review.
     [X] 2 file
  
  Optional review.
  The review obligations of these changes have been satisfied at this point.
  However, you may review them anyway if you desire.
     [ ] 2 other-file

  $ fe session diff -for user2 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | tip * (glob)
  _
  | @@@@@@@@ Hunk 1/2 @@@@@@@@
  | @@@@@@@@ base 1,2 tip 1,2 @@@@@@@@
  | -|file
  | +|first-change
  |_
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ other-file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  _
  | @@@@@@@@ Hunk 2/2 @@@@@@@@
  | @@@@@@@@ base 1,2 tip 1,2 @@@@@@@@
  | -|other-file
  | +|change
  |_

Commit user2's session.  Note that the other file is not part of the line count
shown for user2, since it was not partially reviewed.  This shows that the
partial review has become a file by file criteria.  The page shown by [fe
session show] makes this distinction to guide the user.

  $ id=$(fe session show -id -for user2)
  $ IRON_USER=user2 fe session commit -session-id ${id}
  $ fe show -omit-attribute-table -omit-description
  root/child
  ==========
  
  |---------------------------------------------|
  | user                   | follow | completed |
  |------------------------+--------+-----------|
  | user2                  |      2 |         2 |
  | unix-login-for-testing |        |         4 |
  | user1                  |        |         4 |
  |---------------------------------------------|

  $ fe todo -for user2
  |------------------|
  | feature | follow |
  |---------+--------|
  | root    |        |
  |   child |      2 |
  |------------------|

  $ fe session show -for user2
  Reviewing root/child to e1dfb03d82b0.
  2 files to review: 4 lines total
  
  Follow review.
  Your pending review on these changes does not prevent releasability.
  These files are shown to you just so you can follow along.
     [ ] 2 file
  
  Optional review.
  The review obligations of these changes have been satisfied at this point.
  However, you may review them anyway if you desire.
     [ ] 2 other-file

  $ fe session diff -for user2 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | old tip * | new tip * (glob)
  _
  | @@@@@@@@ Hunk 1/2 @@@@@@@@
  | @@@@@@@@ old tip 1,2 new tip 1,2 @@@@@@@@
  | -|first-change
  | +|second-change
  |_
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ other-file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | tip * (glob)
  _
  | @@@@@@@@ Hunk 2/2 @@@@@@@@
  | @@@@@@@@ base 1,2 tip 1,2 @@@@@@@@
  | -|other-file
  | +|change
  |_

  $ fe show -next-steps
  (Release)

  $ fe release
  $ feature_to_server root

After the release, checks that the partial review lines are due for user2 as
catch-up review.

  $ fe todo -for user2
  |--------------------|
  | feature | catch-up |
  |---------+----------|
  | root    |          |
  |   child |        2 |
  |--------------------|

The diff on the other-file was not registered as part of the diffs to catch-up.

  $ fe catch-up show root/child -for user2 -omit-header -omit-attribute
  Reviewing root/child to e1dfb03d82b0.
  1 files to review: 2 lines total
  
  Catch-up.  The feature was released and you had partial review done on these files.
     [ ] 2 file

  $ fe catch-up diff root/child -for user2 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | old tip * | new tip * (glob)
  @@@@@@@@ old tip 1,2 new tip 1,2 @@@@@@@@
  -|first-change
  +|second-change

  $ IRON_USER=user2 fe catch-up clear root/child
  $ fe todo -for user2

Create another child.

  $ fe create root/child -d child
  $ echo new-state >file
  $ hg com -q -m file
  $ feature_to_server root/child
  $ fe enable
  $ fe second -even-though-owner

  $ fe session mark-file root/child file
  $ IRON_USER=user1 fe session mark-file root/child file
  $ IRON_USER=user2 fe session mark-file root/child file


  $ REVERT_TO_HERE=$(fe show -tip)

  $ echo transient >file; echo transient >other-file
  $ hg com -q -m file
  $ feature_to_server root/child

  $ fe show -omit-attribute-table -omit-description
  root/child
  ==========
  
  |---------------------------------------------|
  | user                   | review | completed |
  |------------------------+--------+-----------|
  | unix-login-for-testing |      4 |         2 |
  | user1                  |      4 |         2 |
  | user2                  |      4 |         2 |
  |---------------------------------------------|

user1 and user2 reviews partially the feature.

  $ IRON_USER=user1 fe session mark-file root/child file
  $ IRON_USER=user2 fe session mark-file root/child file

Revert the feature to a reviewed state, and it's not releasable.  user1 and
user2 have partial review done in their session.

  $ hg revert . -q -r ${REVERT_TO_HERE} ; hg com -m 'revert'
  $ feature_to_server root/child
  $ fe show -next-steps
  (Review)

  $ fe show -omit-attribute-table -omit-description
  root/child
  ==========
  
  |------------------------------------------------------|
  | user                   | review | follow | completed |
  |------------------------+--------+--------+-----------|
  | user1                  |      4 |      2 |         4 |
  | user2                  |      4 |      2 |         4 |
  | unix-login-for-testing |        |        |         2 |
  |------------------------------------------------------|
  
  |-----------------------------|
  | review sessions in progress |
  |-----------------------------|
  | user1                       |
  | user2                       |
  |-----------------------------|

  $ IRON_USER=user1 fe session show
  Warning: the feature has changed since this session was created.  It may be more suitable
  to review the feature to its most recent tip.  Consider committing your session:
  
  |-----------------------------------------------------------------|
  | remaining in session | session end to tip | remaining if commit |
  |----------------------+--------------------+---------------------|
  |                    2 |                  2 |                   0 |
  |-----------------------------------------------------------------|
  
  Reviewing root/child to * (glob)
  1 files to review (1 already reviewed): 4 lines total
     [X] 2 file
     [ ] 2 other-file

user1 forgets the session and user2 commits theirs.  the feature becomes
releasable and user2's review turns into a follow review.

  $ SESSION_ID=$(fe session show -id -for user1)
  $ IRON_USER=user1 fe session lock
  $ IRON_USER=user1 fe session forget -session-id ${SESSION_ID} -file file
  $ IRON_USER=user1 fe session commit -session-id ${SESSION_ID}
  $ IRON_USER=user2 fe session commit -session-id $(fe session show -id -for user2)

  $ fe show -next-steps
  (Release)

  $ fe show -omit-attribute-table -omit-description
  root/child
  ==========
  
  |---------------------------------------------|
  | user                   | follow | completed |
  |------------------------+--------+-----------|
  | user2                  |      2 |         2 |
  | unix-login-for-testing |        |         2 |
  | user1                  |        |         2 |
  |---------------------------------------------|

  $ fe session diff -for user2 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | old tip * | new tip * (glob)
  @@@@@@@@ old tip 1,2 new tip 1,2 @@@@@@@@
  -|transient
  +|new-state

  $ fe show -next-steps
  (Release)

  $ fe todo -for user2
  |------------------|
  | feature | follow |
  |---------+--------|
  | root    |        |
  |   child |      2 |
  |------------------|

  $ fe release
  $ feature_to_server root
  $ fe todo -for user2
  |--------------------|
  | feature | catch-up |
  |---------+----------|
  | root    |          |
  |   child |        2 |
  |--------------------|

  $ fe catch-up diff -for user2 root/child | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | old tip * | new tip * (glob)
  @@@@@@@@ old tip 1,2 new tip 1,2 @@@@@@@@
  -|transient
  +|new-state
