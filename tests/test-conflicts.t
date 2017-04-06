
Start test.

  $ start_test

Setup a repo and a clone

  $ hg init repo
  $ cd repo
  $ remote="$PWD"
  $ touch file1.ml
  $ mkdir subdir
  $ touch subdir/file2.ml
  $ hg add file1.ml subdir/file2.ml
  $ hg commit -m 'init'
  $ cd ..
  $ hg clone repo clone | matches 'updating'
  $ cd clone
  $ clone="$PWD"

Make changes in original repo

  $ cd $remote
  $ echo 'change origin' >file1.ml
  $ echo 'change origin' >subdir/file2.ml
  $ hg commit -m 'changes in original repo'

Make conflicting changes in clone

  $ cd $clone
  $ echo 'change clone' >file1.ml
  $ echo 'change clone' >subdir/file2.ml
  $ hg commit -m 'changes in clone'

Pull to create conflict

  $ hg pull | matches '+1 heads'
  $ hg merge |& matches 'warning: conflicts while merging'
  [1]

Now use [fe conflicts] to display the conflicts

  $ fe conflicts
  file1.ml:1:<<<<<<< local
  subdir/file2.ml:1:<<<<<<< local
  $ cd subdir
  $ fe conflicts
  file2.ml:1:<<<<<<< local
  $ mkdir foo
  $ cd foo
  $ fe conflicts
