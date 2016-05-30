  $ start_test
  $ setup_repo_and_root file
  $ feature_to_server root -fake-valid
  $ cd $HOME
  $ hg init repo2

The rest of the test assumes workspaces are not enabled.

  $ export IRON_OPTIONS='((workspaces false))'

Not in a repo.

  $ ( cd / && fe diff root )
  ("not inside an hg repo" /)
  [1]

In wrong repo.

  $ cd repo2
  $ cat > .hg/hgrc <<EOF
  > [paths]
  > default = ssh://hg//hg/family2/submissions
  > EOF
  $ fe diff root
  Your working directory must be inside a clone of the [root] family,
  but is inside a clone of the [family2] family.
  [1]
  $ fe create root/child -desc desc
  Your working directory must be inside a clone of the [root] family,
  but is inside a clone of the [family2] family.
  [1]
