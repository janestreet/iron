Start test.

  $ start_test

Create hg repos.

  $ basedir=$PWD
  $ mkdir repo1 repo2
  $ cd ${basedir}/repo1
  $ hg init
  $ touch file
  $ hg add file
  $ hg com -m file

  $ cd ${basedir}/repo2
  $ hg init
  $ echo "file2" > file
  $ hg add file
  $ hg com -m file

Create features.

  $ cd ${basedir}/repo1
  $ fe create root1 -description 'root1' -remote-repo-path $(pwd)
  $ cat > .hg/hgrc <<EOF
  >   [paths]
  >   default = ssh://hg//hg/root1/fake-submissions-for-test
  > EOF
  $ fe create root1/foo1 -description 'foo1'
  $ fe create root1/foo2 -description 'foo2'
  $ fe create root1/bar  -description 'root1/bar'

  $ cd ${basedir}/repo2
  $ fe create root2 -description 'root2' -remote-repo-path $(pwd)
  $ cat > .hg/hgrc <<EOF
  >   [paths]
  >   default = ssh://hg//hg/root2/fake-submissions-for-test
  > EOF
  $ fe create root2/bar -description 'root2/bar'

  $ cd ${basedir}/repo1

Show it with full name.

  $ fe show root1 -feature-path
  root1

  $ fe show root1/foo1 -feature-path
  root1/foo1

  $ fe show root1/foo2 -feature-path
  root1/foo2

Show it using partial name only.

  $ fe show foo1 -feature-path
  root1/foo1

  $ fe show foo2 -feature-path
  root1/foo2

  $ fe show foo -feature-path
  ("cannot disambiguate among features" (root1/foo1 root1/foo2))
  [1]

Disambiguate using current repo.

  $ cd /
  $ fe show bar -feature-path
  ("cannot disambiguate among features" (root1/bar root2/bar))
  [1]

  $ cd ${basedir}/repo1
  $ fe show bar -feature-path
  root1/bar

  $ cd ${basedir}/repo2
  $ fe show bar -feature-path
  root2/bar

  $ cd ${basedir}/repo1

A name that is both a full name and a partial name refers to the full name.

  $ fe create root1/root1 -desc 'child of root1 with the same basename'
  $ fe show root1 | head -1
  root1
