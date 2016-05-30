Slurp in the common setup code for the rebase tests:
  $ . $IRON_TEST_DIR/lib/test-rebase-preface.sh &> /dev/null

Commented out material for script debugging:
$ echo "r0=$r0"
> echo "root_tip=$root_tip"
> echo "feature_tip=$feature_tip"
> echo "======Local repo bookmarks:"
> hg --cwd "$local_repo_dir" book
> echo "======Remote repo bookmarks:"
> hg --cwd "$remote_repo_dir" book

-- Test: The rebase should win when valid obligations on both sides:

  $ rb_diamond "$r0" "$root_tip" "$feature_tip" \
  >            -fake-valid-obligations -fake-valid-obligations \
  >   |& matches "merging f1.txt"

  $ hg up -q root/test-feature

We should see the merged file:

  $ cat f1.txt
  a
  feature-insert
  b
  base-insert
  c

This check is too fragile, as I mess with the details of the repos:
xWe should see both parents:
x
x  $ hg book
x  $ hg log
x  $ hg parents -r 3 --template="{rev}\n" | sort -n
x  1
x  2

-- Test: Rebase fails if repo is unclean:

  $ touch unclean.txt
  $ fe rebase root/test-feature |& matches "repository is not clean"
  [1]
  $ rm unclean.txt
