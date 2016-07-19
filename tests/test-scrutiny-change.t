  $ start_test

Here we create a parent feature and a child feature, mark the child
as reviewed, and create a new version of the parent.

  $ hg init repo
  $ cd repo
  $ touch initial; hg add initial; hg commit -m initial

  $ fe create root -remote "$PWD" -d root
  $ feature_to_server root -fake-valid

  $ fe create root/feature -d root/feature
  $ hg -q update -r root/feature
  $ fe enable-review
  $ fe change -add-reviewing file-owner
  $ echo a > for_the_feature; hg add for_the_feature; hg -q commit -m for_the_feature
  $ feature_to_server root/feature -fake-valid
  $ fe session mark-file root/feature for_the_feature
  $ IRON_USER=file-owner fe session mark-file root/feature for_the_feature
  $ fe show
  root/feature
  ============
  root/feature
  
  |-------------------------------------------------------------|
  | attribute              | value                              |
  |------------------------+------------------------------------|
  | next step              | add w-f-reviewer                   |
  | owner                  | unix-login-for-testing             |
  | whole-feature reviewer | unix-login-for-testing             |
  | seconder               | not seconded                       |
  | review is enabled      | true                               |
  | reviewing              | file-owner, unix-login-for-testing |
  | is permanent           | false                              |
  | tip                    | d803b2604c82                       |
  | base                   | 6494d11dd5d8                       |
  |-------------------------------------------------------------|
  
  |------------------------------------|
  | user                   | completed |
  |------------------------+-----------|
  | file-owner             |         2 |
  | unix-login-for-testing |         2 |
  |------------------------------------|

  $ hg -q update -r root
  $ echo a > for_the_base; hg add for_the_base; hg -q commit -m for_the_base
  $ feature_to_server root -fake-valid

Now we rebase the child and bump the scrutiny level of all the files in f2.

  $ scrutiny=$(cat <<EOF
  > ($(fe show root/feature -base) ((scrutiny_level 10)))
  > ($(fe show root/feature -tip ) ((scrutiny_level 10)))
  > EOF
  > )
  $ fe rebase root/feature > /dev/null
  $ scrutiny=$(echo "$scrutiny"; cat <<EOF
  > ($(fe show root/feature -base   ) ((scrutiny_level 10)))
  > ($(hg log -r . --template {node}) ((scrutiny_level 20)))
  > EOF
  > )
  $ feature_to_server root/feature -fake-valid -fake-attribute "$scrutiny"

So now, all files should all be reviewed from scratch (actually even reviewed at all since
the merge is clean), even for files that were never touched in the feature.
If the line count computation changes, note I only care that the line count strictly
decreases when I mark and reaches 0 at the end, the exact numbers don't matter.

  $ fe internal session show-num-lines root/feature
  5
  $ fe session mark-file root/feature for_the_feature
  $ fe internal session show-num-lines root/feature
  3
  $ fe session mark-file root/feature initial
  $ fe internal session show-num-lines root/feature
  2
  $ fe session mark-file root/feature for_the_base
  $ fe internal session show-num-lines root/feature
  0
