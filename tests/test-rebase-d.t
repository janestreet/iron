Slurp in the common setup code for the rebase tests:
  $ . $IRON_TEST_DIR/lib/test-rebase-preface.sh &> /dev/null

Make a rebase that succeeds, but produces a file with conflicts.
We'll move root's f1.txt to one that conflicts with feature's f1.txt.

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

-- Test: rebase produces a file with conflict markers:
  $ cd "$local_repo_dir"
  $ (rb_diamond "$r0" "$root_noncompat" "$feature_tip" \
  >             -fake-valid-obligations -fake-valid-obligations || true ) \
  > |& fgrep -q "conflicts during merge"

  $ cat f1.txt
  a
  <<<<<<< old tip: root/test-feature [0807d98ed66b]
  feature-insert
  ||||||| old base: 0 .* (re)
  =======
  base-conflicts-with-feature-insert
  >>>>>>> new base: root [431dc820ca2a]
  b
  c
