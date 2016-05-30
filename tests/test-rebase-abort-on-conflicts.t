Slurp in the common setup code for the rebase tests:
  $ . $IRON_TEST_DIR/lib/test-rebase-preface.sh &> /dev/null

Make a rebase that produces a file with conflicts.

  $ cd "$remote_repo_dir"
  $ hg up -q -r "$r0"
  $ cat > f1.txt <<EOF
  > a
  > base-conflicts-with-feature-insert
  > b
  > c
  > EOF
  $ hg commit -q -m base-feature-incompatible
  $ root_noncompat=$(hg tip --template={rev})

  $ cd "$local_repo_dir"
  $ hg cat f1.txt -r "$feature_tip"
  a
  feature-insert
  b
  c

Check that the rebase is aborted when the switch -abort-on-merge-conflicts is
supplied and the merge produces a file with conflict markers.

  $ REBASE_OPT="-abort-on-merge-conflicts"
  $ rb_diamond "$r0" "$root_noncompat" "$feature_tip" \
  >             -fake-valid-obligations -fake-valid-obligations \
  >  |& matches "Merge has conflicts and -abort-on-merge-conflicts is provided -- aborting rebase."
  [1]

The tree is back in a clean state after the aborted rebase.

  $ hg st
  $ hg log -r . --template='{node}\n' | sed "s;$feature_tip;\$feature_tip;"
  $feature_tip

  $ cat f1.txt
  a
  feature-insert
  b
  c
