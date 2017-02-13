Start test.

  $ start_test

  $ setup_repo_and_root file
  $ echo change >file; hg com -m change
  $ feature_to_server root -fake-valid
  $ fe enable
  $ fe tools mark-fully-reviewed root -for unix-login-for-testing

Verify that spurious sessions aren't created.
Wait for the session to appear on disk by flushing the serializer events.

  $ for i in {1..10}; do
  >     fe session show 2>/dev/null || true
  >     fe admin server serializer wait-until-synced
  >     ls $IRON_BASEDIR/export/features/*/review-managers/*/review-sessions | wc -l
  > done
  1
  1
  1
  1
  1
  1
  1
  1
  1
  1
