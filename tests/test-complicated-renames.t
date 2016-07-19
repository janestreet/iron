  $ start_test

Creating a diverging rename

  $ hg init repo
  $ cd repo
  $ touch a file_in_root; hg add a file_in_root; hg commit -m a
  $ hg mv a b; echo b > b; hg -q commit -m 'a -> b'
  $ hg -q update -r .^
  $ hg mv a c; hg mv file_in_root file_in_root2; echo c > c; hg -q commit -m 'a -> c'
  $ hg merge; echo cc > c; hg -q commit -m 'diverging renames'
  note: possible conflict - a was renamed multiple times to:
   c
   b
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  (branch merge, don't forget to commit)

Setup the features. The unrelated move of [file_in_root] in the base shouldn't break
the feature.

  $ hg -q update -r 0
  $ fe create root -remote "$PWD" -d d
  $ feature_to_server root -fake-valid
  $ fe create root/feature -d d -tip 1
  $ fe enable
  $ feature_to_server root/feature -fake-valid
  $ fe session mark-file root/feature b
  $ hg -q update -r 2; hg book -f root
  $ feature_to_server root -fake-valid
  $ hg -q update -r 3; hg book -f root/feature
  $ fe change -set-base root root/feature
#  $ IRON_OPTIONS='((verbose (worker)))' feature_to_server root/feature -fake-valid
  $ feature_to_server root/feature -fake-valid
  $ fe show -next-bookmark-update
  No_update_expected

And now review the rebase. We should get an error while renaming, because of the divergence
but right now, it depends on how renames are handled and in fact we review from scratch.

#   $ fe review root/feature | tr '\n' ' '
#   *"Tried to take into account a renaming but couldn't"*(src a) (dst b)* (glob)
