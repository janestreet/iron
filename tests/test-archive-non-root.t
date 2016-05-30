Start test.

  $ start_test

Make a repo with root and child features.

  $ hg init repo
  $ cd repo
  $ remote=$PWD
  $ touch file; hg add file; hg com -m 0
  $ rev0=$(hg log -r . --template {node})
  $ fe create root -remote $remote -d root
  $ fe create root/child -d child

Archive child.

  $ fe archive root/child
  $ fe list -archived -depth max
  |------------------------------------------------------------------------------|
  | feature | feature id                           | archived at                 |
  |---------+--------------------------------------+-----------------------------|
  | root    |                                      |                             |
  |   child | * | * | (glob)
  |------------------------------------------------------------------------------|
