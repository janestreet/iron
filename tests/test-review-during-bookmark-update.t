Checking that we ask the worker for sufficiently many review edges that even if we change
reviewers or reviews during a bookmark update, we won't have troubles.

  $ start_test
  $ setup_repo_and_root a

Make sure we have no review managers, but there is a non empty review goal:

  $ fe change -set-whole-feature-reviewers ''
  $ fe enable
  $ feature_to_server root -fake-valid
  $ echo b > a
  $ hg commit -m b
  $ feature_to_server root -fake-valid
  $ ls $IRON_BASEDIR/export/features/*/review-managers

Now during the next bookmark update, add a reviewer:

  $ echo c > a
  $ hg commit -m c
  $ feature_to_server root -fake-valid -run-between-rpcs '
  >  fe change root -add-whole-feature-reviewer user
  >  IRON_USER=user fe tools mark-fully-reviewed root'

And this reviewer should have a normal review, not a forget followed
by a review from scratch:

  $ IRON_USER=user fe session diff | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ a @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny level10
  base d3873e73d99e | old tip 2ee7dfde2f4c | new tip ca49d1c375e7
  @@@@@@@@ old tip 1,2 new tip 1,2 @@@@@@@@
  -|b
  +|c


Second tricky case: review to the review goal while the update bookmark of a rebase
is pending.

  $ fe create root/child -d 'child' -base 0
  $ fe enable
  $ echo child1 > a
  $ hg commit -q -m child
  $ feature_to_server root/child -fake-valid

Now that the review goal is non empty, rebase and review before the update-bookmark
comes in:

  $ fe rebase |& matches "merging a failed"
  $ echo child2 > a; hg commit -q -m child
  $ feature_to_server root/child -fake-valid -run-between-rpcs '
  >  IRON_USER=unix-login-for-testing fe tools mark-fully-reviewed root/child'

And one would expect a conflict diff4, instead of a forget and a review from scratch:

  $ fe session diff | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ a @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny level10
  old base d3873e73d99e | old tip 511b03935429 | new base ca49d1c375e7 | new tip * (glob)
  @@@@@@@@ View : feature-ddiff @@@@@@@@
  @@@@@@@@ -- old base 1,3 old tip 1,3 @@@@@@@@
  @@@@@@@@ ++ new base 1,3 new tip 1,3 @@@@@@@@
  ---|a
  --+|child1
  ++-|c
  +++|child2
