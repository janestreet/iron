Start test. 

  $ start_test

Setup a repository with a CR-soon.

  $ setup_repo_and_root file
  $ r0=$(hg tip --template={node})
  $ cat > file <<EOF
  > (* $CR-soon user for user1: text *)
  > (* $CR-soon user: text *)
  > EOF
  $ hg commit -m '1'
  $ r1=$(hg tip --template={node})
  $ cat > file <<EOF
  > (* $CR-soon user for user2: text *)
  > (* $CR-soon user: text *)
  > EOF
  $ hg commit -m '2'
  $ fe create root/child -desc 'child' -base $r1
  $ cat > file <<EOF
  > (* $XCR-soon user for user1: text *)
  > (* $CR-soon user: text *)
  > EOF
  $ hg commit -q -m '3'
  $ r3=$(hg tip --template={node})

And the CR-soon shows up when we feed the root to hydra.

  $ feature_to_server root -fake-valid-obligations
  $ fe crs -soon root -for user1
  $ fe crs -soon root -for user2
  file:1:1:
    CR-soon user for user2: text 
  $ fe crs -soon -for file-owner
  file:2:1:
    CR-soon user: text 

And when we feed the child to hydra, the CR-soon doesn't go away, because its text changed.
The implicitly assigned CR-soon does goes away though, because the actual assignee changed.

  $ feature_to_server root/child -fake-valid-obligations -fake-attribute-by-rev "
  > ($r1 ())
  > ($r3 ((owner user1)))
  > "
  $ fe crs -soon root -for user1
  $ fe crs -soon root -for user2
  file:1:1:
    CR-soon user for user2: text 
  $ fe crs -soon -for file-owner
