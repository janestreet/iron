Regression test for the appearance of empty forget.

Start test.

  $ start_test

  $ hg init repo
  $ cd repo
  $ remote=$PWD
  $ echo "hello foo" >file1
  $ echo "hello bar" >file2
  $ hg add file1 file2; hg com -m 0
  $ rev0=$(hg log -r . --template {node})

  $ fe create root -remote $remote -d root -permanent
  $ feature_to_server root -fake-valid
  $ fe enable-review root
  $ fe second root -even-though-owner -even-though-empty

  $ fe create root/child -d child -permanent

  $ echo "hello world" >file1
  $ echo "hello   bar" >file2
  $ hg commit -m 1

  $ feature_to_server root/child -fake-valid

  $ fe enable-review root/child
  $ fe second root/child -even-though-owner
  $ fe tools mark-fully-reviewed root/child

  $ fe release root/child
  $ feature_to_server root -fake-valid
  $ feature_to_server root/child -fake-valid

  $ fe todo
  |---------------------|
  | feature | next step |
  |---------+-----------|
  | root    | release   |
  |---------------------|
  
  Features you own:
  |---------------------|
  | feature | next step |
  |---------+-----------|
  | root    | release   |
  |---------------------|

  $ fe show -omit-attribute-table root/child
  root/child
  ==========
  child

  $ fe session diff root/child
  ("reviewer is up to date, no current session"
   (root/child unix-login-for-testing))
  [1]
