Start test.

  $ start_test

Create a repo.

  $ mkdir repo
  $ cd repo
  $ hg init
  $ echo "1" > f1.ml
  $ hg add f1.ml
  $ hg -q commit -m "1"
  $ rev1=$(hg log -r . --template {node})

Create some features.

  $ fe create root -description 'root' -remote-repo-path $(pwd)
  $ fe create root/a -description 'a'
  $ echo "2" > f2.ml
  $ hg add f2.ml
  $ hg -q commit -m "2"
  $ rev2=$(hg log -r . --template {node})
  $ hg -q up root
  $ echo "3" > f3.ml
  $ hg add f3.ml
  $ hg -q commit -m "3"
  $ rev3=$(hg log -r . --template {node})
  $ echo "4" > f4.ml
  $ hg add f4.ml
  $ hg -q commit -m "4"
  $ feature_to_server root -fake-valid
  $ feature_to_server root/a -fake-valid

The rev graph looks like:

|        1--2
|         \
|          3--4

The feature revs are:

 root base    1
 root tip     4
 root/a base  1
 root/a tip   1

Test rebasing to new bases.

  $ fe rebase root/a -new-base $rev2
  New base (*) must be an ancestor of the parent feature's tip (*). (glob)
  [1]
  $ fe rebase root/a -new-base $rev3 > /dev/null
  $ fe rebase root/a -new-base $rev1
  New base (*) must descend from the feature's current base (*). (glob)
  [1]
