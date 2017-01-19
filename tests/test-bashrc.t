  $ IRON_FUNCTIONAL_TESTING_FORCE_WORKSPACES=true start_test

Create the repo.

  $ mkdir repo
  $ cd repo
  $ hg init
  $ echo "c1" > f1.ml
  $ hg add f1.ml
  $ hg ci -m '.'
  $ remote=$PWD

Create some features.

  $ fe create root -description 'root' -remote $remote
  $ fe create root/child1 -desc child1
  $ fe create root/child2 -desc child2 -owner owner

Define helpers.

  $ function rewrite_pwd { sed "s;$HOME;\$HOME;"; }

  $ function pwd () { builtin pwd | rewrite_pwd; }

Cdw without argument.

  $ cdw && pwd
  $HOME/workspaces

Cdf without argument.

  $ cdf && pwd
  cdf requires you to supply a feature (TAB completion should work).
  [1]

Cdc without argument.

  $ cdc && pwd
  cdc requires you to supply a root feature (TAB completion should work).
  [1]

Cdf - when there was no previous feature.

  $ cdf -
  cdf: OLD_CDF_FEATURE not set
  [1]

Cdf when feature and workspace exist.

  $ cdf root/child1 && pwd
  $HOME/workspaces/root/child1/+share+
  $ cdw

Cdf - when previous workspace exists.

  $ cdf root/child1 && pwd
  $HOME/workspaces/root/child1/+share+
  $ cdf root/child2 && pwd
  $HOME/workspaces/root/child2/+share+
  $ cdf - | rewrite_pwd
  $HOME/workspaces/root/child1/+share+
  $ cdw

Cdf when workspace doesn't exist but feature does.

  $ fe workspace delete root/child1
  $ fe workspace dir root/child1
  ("you don't have a workspace for" root/child1)
  [1]


  $ cdw
  $ cdf root/child1 <(echo 'y') && pwd
  
  $HOME/workspaces/root/child1/+share+

  $ cdw
  $ fe workspace delete root/child1
  $ cdf root/child1 <(echo 'y') && pwd
  
  $HOME/workspaces/root/child1/+share+

Cdf - when previous workspace doesn't exist but feature does.

  $ fe create root/child3 -desc child3
  $ cdf root/child3 && pwd
  $HOME/workspaces/root/child3/+share+
  $ cdf root/child1 && pwd
  $HOME/workspaces/root/child1/+share+
  $ fe workspace delete root/child3
  $ cdf -
  ("you don't have a workspace for" root/child3)
  [1]
  $ fe archive root/child3
  $ cdw

Cdf when neither workspace nor feature exist.

  $ cdf root/not-a-feature
  ("no such feature" root/not-a-feature)
  [1]

Cdf - when neither workspace nor feature exist.

  $ fe create root/child4 -desc child4
  $ cdf root/child4 && pwd
  $HOME/workspaces/root/child4/+share+
  $ cdf root/child1 && pwd
  $HOME/workspaces/root/child1/+share+
  $ fe workspace delete root/child4
  $ fe archive root/child4
  $ cdf -
  ("no such feature" root/child4)
  [1]
  $ cdw

Cdw when feature and workspace exist.

  $ cdw root/child1 && pwd
  $HOME/workspaces/root/child1/+share+

Cdw when workspace doesn't exist but feature does.

  $ cdw && pwd
  $HOME/workspaces
  $ fe workspace delete root/child1
  $ fe workspace dir root/child1
  ("you don't have a workspace for" root/child1)
  [1]

  $ cdw root/child1 <(echo 'Y') && pwd
  
  $HOME/workspaces/root/child1/+share+

Cdw when neither workspace nor feature exist.

  $ cdw root/not-a-feature
  ("no such feature" root/not-a-feature)
  [1]

Cdc when feature exists.

  $ cdc root && pwd
  $HOME/workspaces/root/+clone+

Cdc when feature exists but is not a root feature.

  $ cdc root/child1 |& matches "No such file or directory"
  [1]

Cdc when feature does not exist.

  $ cdc not-a-root |& matches "No such file or directory"
  [1]

Cdw for incomplete path.

  $ cdw t/ch
  ("no such feature" t/ch)
  [1]
  $ cdw child1 && pwd
  $HOME/workspaces/root/child1/+share+
  $ cdw root/ch
  ("cannot disambiguate among features" (root/child1 root/child2))
  [1]

Cdf for incomplete path.

  $ cdf t/ch
  ("no such feature" t/ch)
  [1]
  $ cdf child1 && pwd
  $HOME/workspaces/root/child1/+share+
  $ cdf root/ch
  ("cannot disambiguate among features" (root/child1 root/child2))
  [1]
