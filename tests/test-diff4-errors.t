Start test.

  $ start_test

Setup a repo with two files:

  $ copy_sample_repo two-users ./repo
  $ cd repo
  $ hg init
  $ remote="$PWD"
  $ hg add -q
  $ echo "a" > a
  $ hg commit -m "init"
  $ hg bookmark test

Create the feature

  $ fe create -no-bookmark -tip . test -remote "$remote" -desc 'root for test'
  $ BOOKMARK=test fe internal hydra; hg -q update -r test

  $ fe enable-review test
  $ echo "a'" > a
  $ hg commit -m "change"

  $ BOOKMARK=test fe internal hydra; hg -q update -r test

  $ fe session mark-file test a

#  $ hg log -G -p
  $ hg update -r 0 -q

  $ hg mv a a2
  $ sed -i .fe.sexp -e 's/(files a)/(files a2)/'
  $ hg commit -m "rename" -q

  $ hg merge -r 1 -q
  $ echo "a" > a2
  $ hg commit -m "force 0"

  $ hg bookmark -f test -r .

  $ BOOKMARK=test fe internal hydra; hg -q update -r test

# Check that an expected hydra worker error text appears in the review

