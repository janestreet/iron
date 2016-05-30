Start test.

  $ start_test

Create hg repo.  

  $ mkdir repo
  $ cd repo
  $ hg init
  $ touch a
  $ hg add a
  $ hg commit -m "init"

Create feature.

  $ fe create root -description 'root' -remote-repo-path $(pwd)

Bounce server.

  $ fe-server stop
  $ fe-server start

Feature is still there.

  $ fe list
  |------------------------------------|
  | feature |   lines | next step      |
  |---------+---------+----------------|
  | root    | pending | wait for hydra |
  |------------------------------------|
