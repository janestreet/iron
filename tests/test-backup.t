Start test.

  $ start_test

Set up a repo.

  $ copy_sample_repo two-users ./repo
  $ cd repo
  $ hg init
  $ remote="$PWD"
  $ hg add -q
  $ hg commit -m "init"
  $ seq 1 3 > a
  $ seq 1 5 > b
  $ hg commit -m "add some lines"
  $ hg bookmark test

Create the feature.

  $ fe create -no-bookmark -tip . test -remote "$remote" -desc 'root for test'
  $ BOOKMARK=test fe internal hydra; hg -q update -r test

Enable and review.

  $ fe enable-review test
  $ fe internal session show-num-lines test
  8
  $ fe session show
  Reviewing test to 9bdd8ea4bdfa.
  2 files to review: 8 lines total
     [ ] 3 a
     [ ] 5 b

Backup this state of the world as a reference.

  $ fe admin server serializer pause
  $ TMP=$IRON_APPDIR/tmp
  $ mkdir -p $TMP
  $ cp -R $IRON_BASEDIR/export $TMP/export.1
  $ fe admin server serializer resume

Pause serializer, freeze persistent state.

  $ fe admin server serializer status | head -n 1
  "serializer is not paused"
  $ fe admin server serializer pause
  $ fe admin server serializer status | head -n 1
  ("serializer is paused by"

Do some write operations.

  $ fe session mark-file test a
  $ fe session show
  Reviewing test to 9bdd8ea4bdfa.
  1 files to review (1 already reviewed): 8 lines total
     [X] 3 a
     [ ] 5 b

Check that the file system has not changed.

  $ cp -R $IRON_BASEDIR/export $TMP/export.2
  $ diff -arq $TMP/export.1 $TMP/export.2

Resume persistent state.

  $ fe admin server serializer status | head -n 1
  ("serializer is paused by"
  $ fe admin server serializer resume
  $ fe admin server serializer status | head -n 1
  "serializer is not paused"

Check that the file system is now different.

  $ fe admin server serializer pause && fe admin server serializer resume
  $ cp -R $IRON_BASEDIR/export $TMP/export.3
  $ diff -arq $TMP/export.2 $TMP/export.3 &> /dev/null
  [1]

Persistence after resume.

  $ fe-server stop
  $ fe-server start

  $ fe session show
  Reviewing test to 9bdd8ea4bdfa.
  1 files to review (1 already reviewed): 8 lines total
     [X] 3 a
     [ ] 5 b

Force resume on quit:
Should abort the pause and force flushing the changes.

  $ fe admin server serializer pause
  $ fe session mark-file test b
  $ fe session show |& matches "reviewer is up to date"
  [1]

  $ fe-server stop
  $ fe-server start

  $ fe session show |& matches "reviewer is up to date"
  [1]

One should be able to recover from a backup.

  $ fe-server stop
  $ rm -rf $IRON_BASEDIR/export
  $ cp -R $TMP/export.1 $IRON_BASEDIR/export

  $ fe-server start

  $ fe session show
  Reviewing test to 9bdd8ea4bdfa.
  2 files to review: 8 lines total
     [ ] 3 a
     [ ] 5 b

Simple model: Check that no conflicting backup jobs are allowed.

  $ fe admin server serializer pause
  $ fe admin server serializer pause |& matches "already paused"
  [1]

  $ fe admin server serializer resume
  $ fe admin server serializer resume |& matches "serializer is not paused"
  [1]

Test timeout.

  $ fe admin server serializer pause -with-timeout '5ms'
  $ sleep 0.1
  $ fe admin server serializer status |& matches "serializer is not paused. it timed out"
  $ fe admin server serializer resume |& matches "serializer is not paused, pause timed out"
  [1]

Recover from a timeout.

  $ fe admin server serializer pause
  $ fe admin server serializer status | head -n 1
  ("serializer is paused by"
  $ fe admin server serializer resume
  $ fe admin server serializer status | head -n 1
  "serializer is not paused"

Request a timeout too big.

  $ fe admin server serializer pause -with-timeout '1h' |& matches "requested timeout is too big"
  [1]
  $ fe admin server serializer status | head -n 1
  "serializer is not paused"
