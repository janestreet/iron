Start test.

  $ start_test

Setup repo.

  $ setup_repo_and_root .fe.sexp file other-file user1-f1 user1-f2
  $ mkdir .fe
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Define_scrutiny normal  ((level 50) (description "normal")))
  > (Define_scrutiny ignore  ((level  0) (description "ignore")))
  > (Users unix-login-for-testing user1 user2 file-follower)
  > EOF
  $ touch .fe/obligations-repo.sexp
  $ cat >.fe/.fe.sexp <<EOF
  > (Owner unix-login-for-testing)
  > (Scrutiny ignore)
  > (Apply_to All_files)
  > EOF
  $ cat >.fe.sexp <<EOF
  > (Local
  >   (Followers file-follower)
  >   (Apply_to (Files file)))
  > (Owner user1)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > (Local
  >   (Reviewed_by (At_least 1 of_ (Users user1 user2)))
  >   (Apply_to (Files file other-file)))
  > (Local
  >   (Reviewed_by (All_of (Users user1)))
  >   (Apply_to (Files user1-f1 user1-f2)))
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
  $ echo change >user1-f1; echo change >user1-f2
  $ echo change >file
  $ echo "with whitespaces" >other-file
  $ hg -q com -m file
  $ feature_to_server root/child
  $ fe show -omit-attribute-table -omit-description
  root/child
  ==========
  
  |------------------------------------------|
  | user                   | review | follow |
  |------------------------+--------+--------|
  | unix-login-for-testing |      8 |        |
  | user1                  |      8 |        |
  | user2                  |      4 |        |
  | file-follower          |        |      2 |
  |------------------------------------------|
  $ fe enable
  $ fe second -even-though-owner

Review for unix-login-for-testing, but can't yet release.

  $ fe tools mark-fully-reviewed root/child
  $ fe release |& matches "feature is not releasable.*root/child.*Review"
  [1]

Verify that if the set of wrf(s) is such that once they have reviewed the
feature is releasable, then other users are not showing the lines in their todo.

  $ fe todo -for user1
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |        |
  |   child |      8 |
  |------------------|

  $ fe todo -for user2
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |        |
  |   child |      4 |
  |------------------|

  $ fe show -omit-attribute-table -omit-description
  root/child
  ==========
  
  |------------------------------------------------------|
  | user                   | review | follow | completed |
  |------------------------+--------+--------+-----------|
  | user1                  |      8 |        |           |
  | user2                  |      4 |        |           |
  | file-follower          |        |      2 |           |
  | unix-login-for-testing |        |        |         8 |
  |------------------------------------------------------|

  $ fe change -add-whole-feature-reviewer user1
  $ fe todo -for user1
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |        |
  |   child |      8 |
  |------------------|

  $ fe todo -for user2
  $ fe show -omit-attribute-table -omit-description
  root/child
  ==========
  
  |------------------------------------------------------|
  | user                   | review | follow | completed |
  |------------------------+--------+--------+-----------|
  | user1                  |      8 |        |           |
  | file-follower          |        |      2 |           |
  | unix-login-for-testing |        |        |         8 |
  |------------------------------------------------------|

Verify that even if user1 has an uncommitted session, the file does not flicker
in user2's todo.

  $ IRON_USER=user1 fe session mark-file root/child file

  $ fe todo -for user2
  $ fe show -omit-attribute-table -omit-description
  root/child
  ==========
  
  |------------------------------------------------------|
  | user                   | review | follow | completed |
  |------------------------+--------+--------+-----------|
  | user1                  |      6 |        |         2 |
  | file-follower          |        |      2 |           |
  | unix-login-for-testing |        |        |         8 |
  |------------------------------------------------------|
  
  |-----------------------------|
  | review sessions in progress |
  |-----------------------------|
  | user1                       |
  |-----------------------------|

  $ fe todo -for user2

Verify that if user1 is not a whole feature reviewer, and has an uncommitted
session, the files he reviewed already do not flicker in user2's todo.

  $ IRON_USER=user1 fe session mark-file root/child other-file

  $ IRON_USER=user1 fe session commit root/child \
  >   -session-id $(fe session show -id root/child -for user1)

  $ IRON_USER=user1 fe session mark-file root/child user1-f1

  $ fe change -remove-whole-feature-reviewer user1
  $ fe todo -for user2
  $ fe show -omit-attribute-table -omit-description
  root/child
  ==========
  
  |------------------------------------------------------|
  | user                   | review | follow | completed |
  |------------------------+--------+--------+-----------|
  | user1                  |      2 |        |         6 |
  | file-follower          |        |      2 |           |
  | unix-login-for-testing |        |        |         8 |
  |------------------------------------------------------|
  
  |-----------------------------|
  | review sessions in progress |
  |-----------------------------|
  | user1                       |
  |-----------------------------|

  $ IRON_USER=user2 fe session show root/child
  Reviewing root/child to *. (glob)
  2 files to review: 4 lines total
  
  Optional review.
  The review obligations of these changes have been satisfied at this point.
  However, you may review them anyway if you desire.
     [ ] 2 file
     [ ] 2 other-file

Review for user1 and we can release, even though user2 has review.
Iron does not show the lines for user2 both in [fe show] and in user2's [fe todo].

  $ IRON_USER=user1 fe tools mark-fully-reviewed root/child
  $ fe show -omit-attribute-table -omit-description
  root/child
  ==========
  
  |---------------------------------------------|
  | user                   | follow | completed |
  |------------------------+--------+-----------|
  | file-follower          |      2 |           |
  | unix-login-for-testing |        |         8 |
  | user1                  |        |         8 |
  |---------------------------------------------|

  $ fe todo -for user2

However user2 would be free to review anyway.

  $ fe session show -for user2
  Reviewing root/child to *. (glob)
  2 files to review: 4 lines total
  
  Optional review.
  The review obligations of these changes have been satisfied at this point.
  However, you may review them anyway if you desire.
     [ ] 2 file
     [ ] 2 other-file

Release the feature.

  $ fe release
  $ feature_to_server root

Now the child is included in the root.

  $ fe show
  root
  ====
  root
  
  |---------------------------------------------------------------------|
  | attribute              | value                                      |
  |------------------------+--------------------------------------------|
  | next step              | release                                    |
  | owner                  | unix-login-for-testing                     |
  | whole-feature reviewer | unix-login-for-testing                     |
  | seconder               | unix-login-for-testing (even though owner) |
  | review is enabled      | true                                       |
  | reviewing              | all                                        |
  | is permanent           | true                                       |
  | tip                    | * | (glob)
  | base                   | * | (glob)
  |---------------------------------------------------------------------|
  
  |------------------------------------|
  | user                   | completed |
  |------------------------+-----------|
  | unix-login-for-testing |         8 |
  | user1                  |         8 |
  | user2                  |         4 |
  | file-follower          |         2 |
  |------------------------------------|
  
  Included features:
    root/child

And the diff has been inserted in everyone's brain, including user2 and file-follower.

  $ fe brain show root -for all
  file-follower:
  |--------------------|
  | file | op  | lines |
  |------+-----+-------|
  | file | mod |     2 |
  |--------------------|
  
  unix-login-for-testing:
  |--------------------------|
  | file       | op  | lines |
  |------------+-----+-------|
  | file       | mod |     2 |
  | other-file | mod |     2 |
  | user1-f1   | mod |     2 |
  | user1-f2   | mod |     2 |
  |--------------------------|
  
  user1:
  |--------------------------|
  | file       | op  | lines |
  |------------+-----+-------|
  | file       | mod |     2 |
  | other-file | mod |     2 |
  | user1-f1   | mod |     2 |
  | user1-f2   | mod |     2 |
  |--------------------------|
  
  user2:
  |--------------------------|
  | file       | op  | lines |
  |------------+-----+-------|
  | file       | mod |     2 |
  | other-file | mod |     2 |
  |--------------------------|
  

And user2 has no catch-up generated.

  $ fe todo -for user2 root

Re-create child.

  $ fe create root/child2 -d child2
  $ echo child2 >file; hg com -m file
  $ echo "with    whitespaces" >other-file; hg com -m "white space"
  $ feature_to_server root/child2
  $ fe show -omit-attribute-table -omit-description
  root/child2
  ===========
  
  |------------------------------------------|
  | user                   | review | follow |
  |------------------------+--------+--------|
  | unix-login-for-testing |      2 |        |
  | user1                  |      2 |        |
  | user2                  |      2 |        |
  | file-follower          |        |      2 |
  |------------------------------------------|
  $ fe enable
  $ fe second -even-though-owner

Review for unix-login-for-testing, but can't yet release.

  $ fe tools mark-fully-reviewed root/child2
  $ fe release |& matches "feature is not releasable.*root/child2.*Review"
  [1]

Add a whole feature follower.

  $ fe todo -for user3
  $ fe change -add-whole-feature-followers user3
  $ fe show -whole-feature-followers
  (user3)
  $ fe todo -for user3
  |-------------------|
  | feature  | follow |
  |----------+--------|
  | root     |        |
  |   child2 |      2 |
  |-------------------|
  
  Features you watch:
  |------------------------------|
  | feature  | #left | next step |
  |----------+-------+-----------|
  | root     |       |           |
  |   child2 |     2 | review    |
  |------------------------------|

Review for user1 & user3 up to this point, but push another change.

  $ fe session diff -for user1 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | tip * (glob)
  @@@@@@@@ base 1,2 tip 1,2 @@@@@@@@
  -|change
  +|child2
  $ IRON_USER=user1 fe tools mark-fully-reviewed root/child2
  $ fe brain show -for user1
  |--------------------------|
  | file       | op  | lines |
  |------------+-----+-------|
  | file       | mod |     2 |
  | other-file | mod |       |
  |--------------------------|
  $ IRON_USER=user3 fe tools mark-fully-reviewed root/child2
  $ echo new-contents >file
  $ echo new-contents >other-file
  $ hg com -m file
  $ feature_to_server root/child2

Have user2 actually do the review.

  $ fe tools mark-fully-reviewed root/child2
  $ IRON_USER=user2 fe tools mark-fully-reviewed root/child2

user1 is shown as having to follow some diffs because obligations are satisfied
but they have started reviewing the file.

  $ fe show -omit-attribute-table -omit-description
  root/child2
  ===========
  
  |---------------------------------------------|
  | user                   | follow | completed |
  |------------------------+--------+-----------|
  | user3                  |      4 |         2 |
  | user1                  |      2 |         2 |
  | file-follower          |      2 |           |
  | unix-login-for-testing |        |         4 |
  | user2                  |        |         4 |
  |---------------------------------------------|

It is not allowed to review for a whole-feature follower.  Since they do not
prevent the release there is no value in allowing it.

  $ fe tools mark-fully-reviewed root/child2 -for user3 -reason reason
  (error
   (may-modify-others-review
    ("unauthorized review for a user with only lines to follow" user3)))
  [1]

Same for a file follower.

  $ fe tools mark-fully-reviewed root/child2 -for file-follower -reason reason
  (error
   (may-modify-others-review
    ("unauthorized review for a user with only lines to follow" file-follower)))
  [1]

Since this is going to generate catch-up upon release, Iron encourages user1 to
read this diff.

  $ fe todo -for user1
  |-------------------|
  | feature  | follow |
  |----------+--------|
  | root     |        |
  |   child2 |      2 |
  |-------------------|

However user1 would be free to review all the files anyway.

  $ fe session show -for user1
  Reviewing root/child2 to *. (glob)
  2 files to review: 4 lines total
  
  Follow review.
  Your pending review on these changes does not prevent releasability.
  These files are shown to you just so you can follow along.
     [ ] 2 file
  
  Optional review.
  The review obligations of these changes have been satisfied at this point.
  However, you may review them anyway if you desire.
     [ ] 2 other-file

  $ fe session diff -for user1 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | old tip * | new tip * (glob)
  _
  | @@@@@@@@ Hunk 1/2 @@@@@@@@
  | @@@@@@@@ old tip 1,2 new tip 1,2 @@@@@@@@
  | -|child2
  | +|new-contents
  |_
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ other-file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  _
  | @@@@@@@@ Hunk 2/2 @@@@@@@@
  | @@@@@@@@ old tip, base 1,2 new tip 1,2 @@@@@@@@
  | -|with whitespaces
  | +|new-contents
  |_

The feature is releasable even though user1 and user3 may still review.

  $ fe release
  $ feature_to_server root

And the diff has been inserted in everyone's brain, even user1.

  $ fe brain show root -for user1
  |--------------------------|
  | file       | op  | lines |
  |------------+-----+-------|
  | file       | mod |     2 |
  | other-file | mod |     2 |
  | user1-f1   | mod |     2 |
  | user1-f2   | mod |     2 |
  |--------------------------|
  $ fe brain show root -for user2
  |--------------------------|
  | file       | op  | lines |
  |------------+-----+-------|
  | file       | mod |     2 |
  | other-file | mod |     2 |
  |--------------------------|

This time, user1 has catch-up generated because they had partial review done on
the file.  They have no catch-up on the other-file since they did not even start
to review it.  Note that the white space only diff is not considered as a review
started.

  $ IRON_USER=user1 fe todo
  |---------------------|
  | feature  | catch-up |
  |----------+----------|
  | root     |          |
  |   child2 |        2 |
  |---------------------|

  $ IRON_USER=user1 fe catch-up diff root/child2 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | old tip * | new tip * (glob)
  @@@@@@@@ old tip 1,2 new tip 1,2 @@@@@@@@
  -|child2
  +|new-contents

  $ IRON_USER=user1 fe catch-up show root/child2 -omit-header -omit-attribute-table
  Reviewing root/child2 to *. (glob)
  1 files to review: 2 lines total
  
  Catch-up.  The feature was released and you had partial review done on these files.
     [ ] 2 file

Unlike user1, user3 needs to catch-up on all the files since they were w-f-follower.

  $ IRON_USER=user3 fe todo
  |---------------------|
  | feature  | catch-up |
  |----------+----------|
  | root     |          |
  |   child2 |        4 |
  |---------------------|

  $ IRON_USER=user3 fe catch-up diff root/child2 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | old tip * | new tip * (glob)
  _
  | @@@@@@@@ Hunk 1/2 @@@@@@@@
  | @@@@@@@@ old tip 1,2 new tip 1,2 @@@@@@@@
  | -|child2
  | +|new-contents
  |_
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ other-file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  _
  | @@@@@@@@ Hunk 2/2 @@@@@@@@
  | @@@@@@@@ old tip, base 1,2 new tip 1,2 @@@@@@@@
  | -|with whitespaces
  | +|new-contents
  |_

  $ IRON_USER=user3 fe catch-up show root/child2 -omit-header -omit-attribute-table
  Reviewing root/child2 to cd071a335742.
  2 files to review: 4 lines total
  
  Catch-up.  The feature was released and you were a follower.
     [ ] 2 file
     [ ] 2 other-file

The file follower only has catch-up review on the file they follow.

  $ IRON_USER=file-follower fe catch-up diff root/child2 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | tip * (glob)
  @@@@@@@@ base 1,2 tip 1,2 @@@@@@@@
  -|change
  +|new-contents

  $ IRON_USER=file-follower fe catch-up show root/child2 -omit-header -omit-attribute-table
  Reviewing root/child2 to cd071a335742.
  1 files to review: 2 lines total
  
  Catch-up.  The feature was released and you were a follower.
     [ ] 2 file
