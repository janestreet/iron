Slurp in the common setup code for the rebase tests:
  $ . $IRON_TEST_DIR/lib/test-rebase-preface.sh &> /dev/null

-- Test: Rebase fails when feature or base has bad obligations:
These revsets don't have any obligation metadata -- no .fe or .projections dirs.
So we should observe the rebase refusing to complete unless we specifically
request the server to fake valid obligations.

  $ (rb_diamond "$r0" "$root_tip" "$feature_tip"          \
  >             "" -fake-valid-obligations       || true) \
  > |& fgrep -q "new base obligations are invalid"
