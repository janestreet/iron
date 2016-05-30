
Start test. 

  $ start_test

Setup a repository with a few cr-soons:

  $ hg init repo
  $ cd repo
  $ remote="$PWD"
  $ cat > file1.ml <<EOF
  > (* $CR-soon user1 for user2: cr1 *)
  > (* $CR-soon user1 for user2: cr2 *)
  > (* $CR-soon user1: cr3 bla bla
  >    bla *)
  > (* $CR-soon user1: cr4 *)
  > EOF
  $ hg add file1.ml; hg commit -m live

and a few branches fiddling with cr-soons:

  $ hg book live
  $ hg book live/feature1
  $ sed -i 's/cr1/cr1modified/' file1.ml
  $ sed -i 's/CR-soon user1: cr4/XCR-soon user1: cr4/' file1.ml
  $ echo '(* $CR-soon user1: cr only in feature1 *)' >> file1.ml
  $ hg commit -m 'feature1'
  $ hg -q update -r .^
  $ hg book live/feature2
  $ cat > file1.ml <<EOF
  >   (* $CR-soon user1: cr3 bla
  >      bla bla *)
  >   (* $XCR-soon user1: cr4 *)
  >   (* $CR-soon user1 for user2:   cr1   *)
  >   (* $CR-soon user1 for user2:  cr2 *)
  > EOF
  $ hg -q commit -m 'reindent and shuffle everything in feature2'

For now, let's only consider the base:

  $ hg -q update -r live
  $ fe create -no-bookmark -tip . live -remote "$remote" -desc 'root for live'
  $ fe crs -soon
  $ fe crs -soon -for file-owner
  $ fe crs -soon -for user1
  $ fe crs -soon -for user2

When hydra try to feed information about the base, but the obligations aren't valid,
nothing happens:

  $ BOOKMARK=live fe internal hydra; hg -q update -r live
  $ fe crs -soon
  $ fe crs -soon -for file-owner
  $ fe crs -soon -for user1
  $ fe crs -soon -for user2
  $ fe todo -for user1
  $ fe todo -for user2

But once the obligations are valid, we do get the cr soons:

  $ BOOKMARK=live fe internal hydra -fake-valid-obligations; hg -q update -r live
  $ fe crs -soon
  $ fe crs -soon -for file-owner | grep .
  file1.ml:3:1:
    CR-soon user1: cr3 bla bla
    bla 
  file1.ml:5:1:
    CR-soon user1: cr4 
  $ fe crs -soon -for user1
  $ fe crs -soon -for user2 | grep .
  file1.ml:1:1:
    CR-soon user1 for user2: cr1 
  file1.ml:2:1:
    CR-soon user1 for user2: cr2 
  $ fe crs -soon -for all | grep .
  file1.ml:1:1:
    CR-soon user1 for user2: cr1 
  file1.ml:2:1:
    CR-soon user1 for user2: cr2 
  file1.ml:3:1:
    CR-soon user1: cr3 bla bla
    bla 
  file1.ml:5:1:
    CR-soon user1: cr4 
  $ fe todo -for user1
  $ fe todo -for user2
  CR-soons assigned to you:
  |---------------------|
  | family | file:line  |
  |--------+------------|
  | live   | file1.ml:1 |
  | live   | file1.ml:2 |
  |---------------------|

When live/feature1 is created, some other cr-soons aren't shown anymore.

  $ hg -q update -r live/feature1
  $ fe create -no-bookmark -tip . live/feature1 -desc 'feature1'
  $ BOOKMARK=live/feature1 fe internal hydra -fake-valid-obligations; hg -q update -r live/feature1
  $ fe crs -soon
  $ fe crs -soon -for file-owner | grep .
  file1.ml:3:1:
    CR-soon user1: cr3 bla bla
    bla 
  $ fe crs -soon -for user1
  $ fe crs -soon -for user2 | grep .
  file1.ml:2:1:
    CR-soon user1 for user2: cr2 
  $ fe crs -soon -for user2 -include-active | grep .
  file1.ml:1:1:
  active in : live/feature1
    CR-soon user1 for user2: cr1 
  file1.ml:2:1:
    CR-soon user1 for user2: cr2 
  $ fe crs -soon -for all | grep .
  file1.ml:2:1:
    CR-soon user1 for user2: cr2 
  file1.ml:3:1:
    CR-soon user1: cr3 bla bla
    bla 
  $ fe crs -soon -for all -include-active | grep .
  file1.ml:1:1:
  active in : live/feature1
    CR-soon user1 for user2: cr1 
  file1.ml:2:1:
    CR-soon user1 for user2: cr2 
  file1.ml:3:1:
    CR-soon user1: cr3 bla bla
    bla 
  file1.ml:5:1:
  active in : live/feature1
    CR-soon user1: cr4 
  $ fe todo -for user2 -include-active
  CR-soons assigned to you:
  |-------------------------------------|
  | family | file:line  | active in     |
  |--------+------------+---------------|
  | live   | file1.ml:1 | live/feature1 |
  | live   | file1.ml:2 |               |
  |-------------------------------------|

When live/feature2 is created though, the change of indentation is not
enough to consider that the crs are active.

  $ hg -q update -r live/feature2
  $ fe create -no-bookmark -tip . live/feature2 -desc 'feature2'
  $ BOOKMARK=live/feature2 fe internal hydra -fake-valid-obligations; hg -q update -r live/feature2
  $ fe crs -soon
  $ fe crs -soon -for file-owner | grep .
  file1.ml:3:1:
    CR-soon user1: cr3 bla bla
    bla 
  $ fe crs -soon -for user1
  $ fe crs -soon -for user2 | grep .
  file1.ml:2:1:
    CR-soon user1 for user2: cr2 
  $ fe crs -soon -for all | grep .
  file1.ml:2:1:
    CR-soon user1 for user2: cr2 
  file1.ml:3:1:
    CR-soon user1: cr3 bla bla
    bla 

If I delete a file entirely, its cr-soons are deleted:

  $ hg -q update -r live 
  $ hg book 'live/feature3'
  $ hg rm file1.ml
  $ hg -q commit -m 'feature3'
  $ fe create -no-bookmark -tip . live/feature3 -desc 'feature3'
  $ BOOKMARK=live/feature3 fe internal hydra -fake-valid-obligations; hg -q update -r live/feature3
  $ fe crs -soon
  $ fe crs -soon -for file-owner
  $ fe crs -soon -for user1
  $ fe crs -soon -for user2
  $ fe crs -soon -for all

And when the feature is archived, we get the active cr-soons back:

  $ fe archive live/feature3
  $ fe crs live -soon -for user2 | grep .
  file1.ml:2:1:
    CR-soon user1 for user2: cr2 

If I create a friend repo:

  $ hg init friend
  $ hg cat -r live file1.ml > friend/file1.ml
  $ cd friend
  $ hg add file1.ml
  $ hg commit -m file1
  $ hg book friend
  $ fe create friend -no-bookmark -tip . -remote "$remote" -desc 'root for friend'
  $ BOOKMARK=friend fe internal hydra -fake-valid-obligations; hg -q update -r friend
  $ fe crs live -soon -for user2 | grep .
  file1.ml:2:1:
    CR-soon user1 for user2: cr2 
  $ fe crs friend -soon -for user2 | grep .
  file1.ml:1:1:
    CR-soon user1 for user2: cr1 
  file1.ml:2:1:
    CR-soon user1 for user2: cr2 

Features in the friend repo only impact cr-soons in the friend repo, not
cr-soons in live:

  $ hg book friend/feature1
  $ > file1.ml
  $ hg commit -m 'friend1'
  $ fe create friend/feature1 -no-bookmark -tip . -desc 'root for live'
  $ BOOKMARK=friend/feature1 fe internal hydra -fake-valid-obligations; hg -q update -r friend/feature1
  $ fe crs live -soon -for user2 | grep .
  file1.ml:2:1:
    CR-soon user1 for user2: cr2 
  $ fe crs friend -soon -for user2

cr-soons are persistent:

  $ fe-server stop
  $ fe-server start
  $ fe crs live -soon -for user2 -include-active | grep .
  file1.ml:1:1:
  active in : live/feature1
    CR-soon user1 for user2: cr1 
  file1.ml:2:1:
    CR-soon user1 for user2: cr2 

Active cr-soons are persistent:

  $ fe crs live -soon -for user2 | grep .
  file1.ml:2:1:
    CR-soon user1 for user2: cr2 
