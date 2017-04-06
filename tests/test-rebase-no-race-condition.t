Slurp in the common setup code for the rebase tests:
  $ . $IRON_TEST_DIR/lib/test-rebase-preface.sh &> /dev/null

  $ cd "$remote_repo_dir"

  $ export temp_dir="$(mktemp -d --tmpdir)"
  $ trap 'exit_trap; rm -rf "$temp_dir"' EXIT

  $ function run_after_push() {
  >   fe show root/test-feature -display-ascii    > $temp_dir/show.txt
  >   fe show root/test-feature -next-base-update > $temp_dir/next-base-update.txt
  >   feature_to_server root/test-feature -fake-valid
  > }

  $ export -f run_after_push

  $ export IRON_FUNCTIONAL_TESTING_REBASE_RACE=run_after_push

  $ rb_diamond "$r0" "$root_tip" "$feature_tip" \
  >            -fake-valid-obligations -fake-valid-obligations \
  >  |& matches "rejecting hydra worker query when a base update is expected"

  $ fe show root/test-feature -next-base-update
  No_update_expected

  $ cat $temp_dir/show.txt | grep '^| base' | single_space
  | base | none (last known as * * ago) | (glob)

  $ cat $temp_dir/next-base-update.txt | sexp print -machine
  (Update_expected((rev *)(by unix-login-for-testing)(expected_since(*)))) (glob)
