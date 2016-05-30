Slurp in the common setup code for the rebase tests:
  $ . $IRON_TEST_DIR/lib/test-rebase-preface.sh &> /dev/null

Let's observe things failing when either the feature or base has conflict markers.

-- Test: rebase fails when conflict markers in base:
  $ (rb_diamond "$r0" "$root_conflict" "$feature_tip" \
  >             -fake-valid-obligations -fake-valid-obligations || true ) \
  > |& fgrep -q "new base is not conflict free"

-- Test: rebase fails when conflict markers in feature:
  $ (rb_diamond "$r0" "$root_tip" "$feature_conflict" \
  >             -fake-valid-obligations -fake-valid-obligations || true ) \
  > |& fgrep -q "feature is not conflict free"
