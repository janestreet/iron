Slurp in the common setup code for the rebase tests:
  $ . $IRON_TEST_DIR/lib/test-rebase-preface.sh &> /dev/null

-- Test: Commit metadata gets in on successful rebases.

  $ rb_diamond "$r0" "$root_tip" "$feature_tip" \
  >    -fake-valid-obligations -fake-valid-obligations \
  >  |& matches "merging f1.txt"

  $ hg log -r . --debug | matches "original_bookmark=root/test-feature"
  $ hg log -r . --debug | matches "iron_feature_id="
