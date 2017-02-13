  $ start_test

Make a review manager where the brain matches the goal

  $ setup_repo_and_root file
  $ echo a > file; hg commit -m a
  $ feature_to_server root -fake-valid
  $ fe tools mark-fully-reviewed root -for unix-login-for-testing

Make sure the brain from marking is on disk.

  $ fe admin server serializer wait-until-synced

And now check that bouncing the server doesn't rewrite that brain:

  $ brain_file=$IRON_BASEDIR/export/features/$(fe show -id)/review-managers/unix-login-for-testing/known
  $ before=$(stat --format %i $brain_file)
  $ fe-server stop
  $ fe-server start
  $ fe admin server serializer wait-until-synced
  $ after=$(stat --format %i $brain_file)
  $ diff -u <(echo "$before") <(echo "$after")
