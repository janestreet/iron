Start test.

  $ start_test

Make a repo with root and child features.

  $ hg init repo
  $ cd repo
  $ remote=$PWD
  $ touch file; hg add file; hg com -m 0
  $ rev0=$(hg log -r . --template {node})
  $ fe create root -remote $remote -d root -permanent
  $ echo a > file; hg commit -m 1
  $ feature_to_server root -fake-valid
  $ fe create root/child -d child
  $ fe change -set-property key=value

Start trying to release.

  $ fe is-releasable |& matches "feature is not releasable.*Wait_for_hydra"
  [1]
  $ feature_to_server root/child -fake-valid
  $ fe is-releasable |& matches "feature is not releasable.*Add_code"
  [1]
  $ echo change >file; hg com -m 2
  $ rev2=$(hg log -r . --template {node})
  $ feature_to_server root/child -fake-valid
  $ fe is-releasable |& matches "feature is not releasable.*Enable_review"
  [1]
  $ fe change -add-whole-feature-reviewers seconder
  $ fe enable-review
  $ fe change -set-reviewing-whole-feature-only
  $ fe is-releasable |& matches "feature is not releasable.*Review"
  [1]
  $ fe tools mark-fully-reviewed root/child -for unix-login-for-testing
  $ fe is-releasable |& matches "feature is not releasable.*Review"
  [1]
  $ fe tools mark-fully-reviewed root/child -for seconder -reason reason
  $ fe is-releasable |& matches "feature is not releasable.*Ask_seconder"
  [1]
  $ IRON_USER=seconder fe second
  $ fe release |& matches "feature is not releasable.*In_parent Enable_review"
  [1]
  $ fe change root -add-whole-feature-reviewers seconder
  $ fe enable-review root
  $ fe release |& matches "feature is not releasable.*In_parent Review"
  [1]
  $ fe tools mark-fully-reviewed root -for unix-login-for-testing
  $ fe widen-reviewing root
  $ fe release |& matches "feature is not releasable.*In_parent Review"
  [1]
  $ fe tools mark-fully-reviewed root -for seconder -reason reason
  $ fe catch-up mark-file root file -for seconder
  $ fe release |& matches "feature is not releasable.*In_parent Ask_seconder"
  [1]
  $ IRON_USER=seconder fe second root

Finally, the feature is releasable.

  $ fe is-releasable

But not releasable if it's locked.

  $ fe lock root/child -release -reason 'test'
  $ fe show root/child -what-is-locked |& matches "Release.*(reason test)"
  $ fe release root/child |& matches "feature is not releasable.*Unlock Release"
  [1]

Check persistence of lock.

  $ fe-server stop
  $ fe-server start
  $ fe release root/child |& matches "feature is not releasable.*Unlock Release"
  [1]
  $ fe unlock root/child -release
  $ fe show root/child -what-is-locked
  ()

And not releasable if the parent is locked.

  $ fe lock root -release-into -reason 'test'
  $ fe release root/child \
  >     |& matches "feature is not releasable.*In_parent (Unlock Release_into)"
  [1]
  $ fe unlock root -release-into
  $ fe show root -what-is-locked
  ()

Tag child tip.

  $ hg tag -f -r $(fe show root/child -tip) 'root-000.00'
  $ fe update root/child

Look for releasable features in the todo.

  $ fe todo -releasable
  |---------------------|
  | feature | next step |
  |---------+-----------|
  | root    | release   |
  |   child | release   |
  |---------------------|
  
  Features you own:
  |---------------------|
  | feature | next step |
  |---------+-----------|
  | root    | release   |
  |   child | release   |
  |---------------------|

  $ fe todo -releasable-names
  root
  root/child

Save the diff as of prior to release.

  $ THE_DIFF=$(fe diff -even-ignored root/child | fe internal remove-color)
  $ echo "${THE_DIFF}"
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny level10
  base * | tip * (glob)
  @@@@@@@@ base 1,2 tip 1,2 @@@@@@@@
  -|a
  +|change

Check that Iron won't send a release email to the parent's recipients.

  $ fe change root       -add-send-email-to emails-for-root
  $ fe change root/child -add-send-email-to emails-for-child

  $ fe tools get-feature-email-recipients root/child -sent-upon release
  emails-for-child
  seconder
  unix-login-for-testing

  $ fe change root       -remove-send-email-to emails-for-root
  $ fe change root/child -remove-send-email-to emails-for-child

Release.  Make sure that the next step is recomputed freshly in case it is corrupted.

  $ fe internal cached-attributes force-set root/child -skip-post-rpc-check -next-steps '(Add_code)'
  $ fe show -next-steps root/child
  (Add_code)
  $ fe release root/child

After release
=============

The child bookmark is gone, and the parent bookmark is active.

  $ hg book
   * root                      2:ef5fcbdf8858

The child feature is gone.

  $ feature_to_server root -fake-valid
  $ fe list
  |-----------------------------|
  | feature | lines | next step |
  |---------+-------+-----------|
  | root    |     1 | release   |
  |-----------------------------|

The root tip has been updated.

  $ [ $(fe show root -tip) = $rev2 ]

There is no review to do in the root.

  $ fe show -show-included-feature-details
  root
  ====
  root
  
  |------------------------------------------------------------|
  | attribute               | value                            |
  |-------------------------+----------------------------------|
  | next step               | release                          |
  | owner                   | unix-login-for-testing           |
  | whole-feature reviewers | seconder, unix-login-for-testing |
  | seconder                | seconder                         |
  | review is enabled       | true                             |
  | reviewing               | all                              |
  | is permanent            | true                             |
  | tip                     | root-000.00 [ef5fcbdf8858]       |
  | base                    | 6af58578f44e                     |
  |------------------------------------------------------------|
  
  |------------------------------------|
  | user                   | completed |
  |------------------------+-----------|
  | seconder               |         1 |
  | unix-login-for-testing |         1 |
  |------------------------------------|
  
  Included features:
    root/child
  
  root/child
  ==========
  child
  
  |----------------------------------------------------------------|
  | attribute               | value                                |
  |-------------------------+--------------------------------------|
  | id                      | * | (glob)
  | owner                   | unix-login-for-testing               |
  | whole-feature reviewers | seconder, unix-login-for-testing     |
  | seconder                | seconder                             |
  | tip                     | root-000.00 [ef5fcbdf8858]           |
  | base                    | 88de6830d27e                         |
  | key                     | value                                |
  |----------------------------------------------------------------|

The todo is updated.

  $ fe todo -releasable-names
  root

Check that one can still get the diff of a released feature.
This should be the diff as of prior to the last release, not the empty diff.

  $ diff \
  >  <(echo "${THE_DIFF}") \
  >  <(fe diff -even-ignored -archived root/child | fe internal remove-color)

Check that other feature properties reflect the most recent state of the feature.

  $ fe unarchive root/child
  $ fe change -set-description "description changed after initial archive"
  $ fe archive root/child
  $ fe description show -archived root/child
  description changed after initial archive

Persistence.

  $ fe-server stop
  $ fe-server start

And again.  This used to trigger a bug, when the server would
mistakenly dupe some information in the state deserializer.

  $ fe-server stop
  $ fe-server start

Tag root tip.

  $ hg up -q -r 0
  $ hg tag -f -r $(fe show root -tip) 'root-123.45'
  $ fe update root

Release root.

  $ fe release
  $ feature_to_server root -fake-valid
  $ fe show
  root
  ====
  root
  
  |------------------------------------------------------------|
  | attribute               | value                            |
  |-------------------------+----------------------------------|
  | next step               | add code                         |
  | owner                   | unix-login-for-testing           |
  | whole-feature reviewers | seconder, unix-login-for-testing |
  | seconder                | seconder                         |
  | review is enabled       | true                             |
  | reviewing               | all                              |
  | is permanent            | true                             |
  | tip                     | root-123.45 [ef5fcbdf8858]       |
  | base                    | root-123.45 [ef5fcbdf8858]       |
  |------------------------------------------------------------|

Releasing by a user that owns the parent but not the child.

  $ fe create root/child -owner user1 -desc description
  $ echo b >file; hg com -q -m commit
  $ feature_to_server root/child -fake-valid
  $ fe tools mark-fully-reviewed root/child -for user1 -reason reason
  $ fe enable-review
  $ IRON_USER=user1 fe second -even-though-owner
  $ [ $(fe show -owner root) != $(fe show -owner root/child) ]
  $ fe release root/child
  $ fe list root -name-only
  root
