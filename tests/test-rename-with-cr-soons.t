Start test.

  $ start_test
  $ setup_repo_and_root file

Add a CR-soon at root tip.

  $ cat >file1.ml <<EOF
  > (* $CR-soon user1: cr *)
  > EOF
  $ hg add file1.ml; hg -q commit -m "0"
  $ feature_to_server root -fake-valid
  $ fe crs -soon -for file-owner
  file1.ml:1:1:
    CR-soon user1: cr 

XCR-soon and it is no longer shown.

  $ fe create root/crs -description 'crs'
  $ cat >file1.ml <<EOF
  > (* $XCR-soon user1: cr *)
  > EOF
  $ hg -q commit -m "1"
  $ feature_to_server root/crs -fake-valid
  $ fe crs -soon -for file-owner
  $ fe crs -soon -for file-owner -include-active
  file1.ml:1:1:
  active in : root/crs
    CR-soon user1: cr 

Rename, and the CR-soon is active in the renamed feature.

  $ fe rename root/crs root/crs2
  $ fe show -feature-path
  root/crs2
  $ fe crs -soon -for file-owner
  $ fe crs -soon -for file-owner -include-active
  file1.ml:1:1:
  active in : root/crs2
    CR-soon user1: cr 

Persist, and the CR-soon is still active in the renamed feature.

  $ fe-server stop
  $ fe-server start
  $ fe crs -soon -for file-owner
  $ fe crs -soon -for file-owner -include-active
  file1.ml:1:1:
  active in : root/crs2
    CR-soon user1: cr 
