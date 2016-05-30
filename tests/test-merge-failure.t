  $ start_test

Create a conflict where the child changes a file while the base deletes it:

  $ setup_repo_and_root file
  $ feature_to_server root -fake-valid
  $ fe create root/child -d ''
  $ echo changed > file
  $ hg commit -m child
  $ feature_to_server root/child -fake-valid
  $ hg -q update -r root
  $ hg rm file
  $ hg -q commit -m root
  $ feature_to_server root -fake-valid

And now rebase fails:

  $ fe show root/child -base
  dc568be383d74aa2aadbbcea8df3869d792d7ff4
  $ fe rebase root/child |& matches "use (c)hanged version or (d)elete? abort: response expected"
  [1]
  $ fe show root/child -base
  dc568be383d74aa2aadbbcea8df3869d792d7ff4

Unless you provide input:

  $ echo d | fe rebase root/child |& matches "branch merge, don't forget to commit"
  $ fe show root/child -base
  a4069a5ed8f9c61ba0da2b3be40de31223f7ac2e
  $ fe show root -tip
  a4069a5ed8f9c61ba0da2b3be40de31223f7ac2e
