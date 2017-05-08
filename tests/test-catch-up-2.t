Start test.

  $ start_test

Setup a repo

  $ hg init repo
  $ cd repo
  $ touch a; hg add a
  $ hg commit -m "init"
  $ fe create test-root -remote "$PWD" -desc 'root for test'
  $ fe change -add-whole-feature-reviewers user2
  $ fe enable-review test-root
  $ feature_to_server test-root -fake-valid-obligations
  $ fe tools mark-fully-reviewed test-root -for all -reason reason
  $ fe second test-root -even-though-owner -even-though-empty

Release a child into test-root

  $ fe create test-root/feat1 -desc "child feature desc"
  $ fe change -add-whole-feature-reviewers user1
  $ fe enable-review test-root/feat1
  $ fe change -set-reviewing-whole-feature-only
  $ seq 1 3 > a
  $ hg commit -m "add some lines"
  $ feature_to_server test-root/feat1 -fake-valid-obligations
  $ fe session  mark-file test-root/feat1 a -for user1 -reason reason
  $ fe catch-up mark-file test-root/feat1 a -for user1
  $ fe session  mark-file test-root/feat1 a
  $ IRON_USER=user1 fe second test-root/feat1
  $ fe release test-root/feat1
  $ feature_to_server test-root -fake-valid-obligations

And now, the reviewers of test-root feature, ie user2, should have neither review nor
catch-up, despite the implicit marking that happens.

  $ fe internal session  show-num-lines test-root -for user2
  0
  $ fe internal catch-up show-num-lines test-root -for user2
  0
