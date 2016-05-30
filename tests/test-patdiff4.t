  $ start_test

Patdiff4 infers revisions by grepping for conflict markers in the
source file.  This test detects when its regexes no longer match the
conflict markers inserted by [fe rebase].

  $ mkdir repo; hg init repo; cd repo
  $ echo old-base > f.txt; hg add f.txt; hg ci -m '.'

  $ fe create root -desc 'root' -remote-repo-path $(pwd)
  $ feature_to_server root -fake-valid-obligations

  $ fe create root/child -desc 'child'
  $ echo old-tip > f.txt; hg ci -m '.'
  $ feature_to_server root/child -fake-valid-obligations

  $ hg update -r root -q
  $ echo new-base > f.txt; hg ci -m '.' -q
  $ feature_to_server root -fake-valid-obligations

  $ fe rebase root/child; hg revert -r root f.txt; hg ci -m merge
  merging f.txt
  merge: warning: conflicts during merge
  merging f.txt failed!
  0 files updated, 0 files merged, 0 files removed, 1 files unresolved
  use 'hg resolve' to retry unresolved file merges or 'hg update -C .' to abandon

The rev ids of rebases are not stable because fe inserts the
iron_feature_id (a uuid) into the extras.  So don't compare them.

  $ patdiff4 auto diff f.txt | fe internal remove-color |
  > sed 's/3:1,2   [0-9a-f]\{12\}/3:1,2   ____________/'
  o    3:1,2   ____________   1970-01-01 00:00 +0000   test
  |\     root/child: rebase to 614803c09d62 with ancestor ce7e9b61f7f9
  | |
  | o  2[root]:0   614803c09d62   1970-01-01 00:00 +0000   test
  | |    .
  | |
  o |  1   5dfc57a5e962   1970-01-01 00:00 +0000   test
  |/     .
  |
  @@@@@@@@ A change in the feature was reverted @@@@@@@@
  @@@@@@@@ old tip 1,2 base, new tip 1,2 @@@@@@@@
  -|old-tip
  +|new-base
 
