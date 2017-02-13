  $ start_test

Usees a repo with revs like this:

# r1   r2
#   \  /
#    r0

  $ setup_repo_and_root file
  $ feature_to_server root -fake-valid
  $ fe enable
  $ fe second -even-though-owner -even-though-empty
  $ r0=$(hg tip --template={node})

Make a releasable child.

  $ fe create root/child -d child
  $ echo change >file; hg commit -m change
  $ r1=$(hg tip --template={node})
  $ feature_to_server root/child -fake-valid
  $ fe enable
  $ fe tools mark-fully-reviewed child -reason reason
  $ fe second -even-though-owner

Set the root bookmark to r2, from which r1 does not descend.

  $ hg up -q -r $r0
  $ echo change2 >file; hg commit -q -m change2
  $ r2=$(hg tip --template={node})
  $ hg book root -f -r $r2

Create a local repository, and release the child -- the release fails.

  $ [ $(hg log -r root --template={node}) = $r2 ]
  $ hg -q clone . ../local
  $ ( cd ../local && hg book -d root && fe release child )
  ("Failed to release feature" root/child
   ((feature_base dc568be383d7) (parent_tip 28da2ad87d67)))
  [1]
  $ [ $(hg log -r root --template={node}) = $r2 ]
