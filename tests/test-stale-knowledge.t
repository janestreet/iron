  $ start_test

  $ setup_repo_and_root a
  $ fe create root/feature -d root/feature
  $ touch b; hg add b; hg commit -m b
  $ fe enable
  $ feature_to_server root/feature -fake-valid
  $ fe session show
  Reviewing root/feature to c76fab53bb6c.
  1 files to review: 1 lines total
     [ ] 1 b
  $ fe session mark-file root/feature b
  $ fe session show |& matches "reviewer is up to date"
  [1]
  $ hg rm b
  $ hg commit -m b2
  $ feature_to_server root/feature -fake-valid
  $ fe session show
  Reviewing root/feature to eba0d686abac.
  1 files to review: 1 lines total
     [ ] 1 b
  $ fe session diff | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ b @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  base file    = <absent>
  old tip file = b
  new tip file = <absent>
  base d3873e73d99e | old tip c76fab53bb6c | new tip eba0d686abac
  @@@@@@@@ A change in the feature was reverted @@@@@@@@
  @@@@@@@@ old tip 1,5 base, new tip 1,2 @@@@@@@@
  -|file        = b
  -|scrutiny    = level10
  -|owner       = file-owner
  -|reviewed by = None
  +|<absent>
