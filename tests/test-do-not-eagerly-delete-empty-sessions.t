  $ start_test

  $ setup_repo_and_root file
  $ fe enable

  $ echo a > file; hg commit -m a
  $ feature_to_server root -fake-valid
  $ id=$(fe session show -id)

Checking that when a race happens between reviewing the first file in a session and a
bookmark update, the bookmark update doesn't delete the empty session

  $ echo b > file; hg commit -m a
  $ feature_to_server root -fake-valid
  $ fe internal session mark-id root -session-id $id
