Start test.

  $ start_test

Create repo.

  $ setup_repo_and_root file

Create some features.

  $ fe create root/a -description 'a'

  $ feature_to_server root -fake-valid
  $ feature_to_server root/a -fake-valid

  $ fe enable-review
  $ fe second -even-though-owner -even-though-empty

  $ fe create root/a/aa -description 'aa'
  $ echo "file in aa" > file
  $ hg commit -m "aa"
  $ feature_to_server root/a/aa -fake-valid

  $ fe enable-review root/a/aa
  $ fe second -even-though-owner root/a/aa
  $ fe tools mark-fully-reviewed root/a/aa -for all -reason reason

  $ fe create root/b -description 'b'
  $ fe create root/c -description 'c'

  $ feature_to_server root -fake-valid
  $ feature_to_server root/a -fake-valid
  $ feature_to_server root/b -fake-valid
  $ feature_to_server root/c -fake-valid
  $ feature_to_server root/a/aa -fake-valid

  $ fe todo
  |---------------------|
  | feature | next step |
  |---------+-----------|
  | root    |           |
  |   a     |           |
  |     aa  | release   |
  |---------------------|
  
  Features you own:
  |---------------------|
  | feature | next step |
  |---------+-----------|
  | root    |           |
  |   a     |           |
  |     aa  | release   |
  |   b     | add code  |
  |   c     | add code  |
  |---------------------|

Set somethings to be permanent.

  $ fe change root -set-is-permanent true
  $ fe change root/b -set-is-permanent true

  $ fe lock root/a/aa -release -reason 'not meant to be released' -permanent
  $ fe show root/a/aa -next-steps
  ((Unlock Release))

  $ fe lock root/c -release -reason 'not meant to be released' -permanent

The permanent umbrella features as weel as permanently locked features are omitted
from the user's todo.

  $ fe todo

Check that this behavior is persisted.

  $ fe-server stop
  $ fe-server start

  $ fe todo

Insist on displaying next steps.

  $ fe todo -include-all-owned-features
  Features you own:
  |--------------------------|
  | feature | next step      |
  |---------+----------------|
  | root    | add code       |
  |   a     | add code       |
  |     aa  | unlock release |
  |   b     | add code       |
  |   c     | add code       |
  |--------------------------|
