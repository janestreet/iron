Slurp in the common setup code for the rebase tests:

  $ . $IRON_TEST_DIR/lib/test-rebase-preface.sh &>/dev/null

The next test makes sure we catch developers doing brain-dead things by accident.

When the feature_to_server shell-function magic does the
:   fe change root/test-feature -set-base $root_tip"
this will croak, as this revset is not in the local repo, and this command won't
pull for you. So we pull all the remote-repo stuff over before this test.

  $ hg pull -q "$remote_repo_dir"

-- Test: Fail if old base isn't an ancestor of new base:

  $ (rb_diamond "$root_tip" "$r0" "$feature_tip" \
  >             -fake-valid-obligations -fake-valid-obligations || true) \
  > |& fgrep -q "feature's base is unexpectedly not the greatest-common ancestor of the feature and the new base"

Make the feature reasonable for rebasing.

  $ fe change root -set-base $r0
  $ hg book   root -f -r $root_tip
  $ feature_to_server root -fake-valid
  $ fe change root/test-feature -set-base $r0
  $ hg book   root/test-feature --f -r $feature_tip
  $ feature_to_server root/test-feature -fake-valid

-- Test: Fail if user is not owner or reviewer of feature

  $ fe rebase root/test-feature -for some-random-user
  (error
   (prepare-to-rebase (Failure "some-random-user is not allowed to rebase")))
  [1]

Lock rebase

  $ fe lock -rebase -reason 'test'
  $ fe show -what-is-locked |& matches "Rebase.*(reason test)"
  $ fe rebase root/test-feature \
  >     |& matches "locked.*(feature_path root/test-feature).*(lock_name Rebase)"
  [1]
  $ REV=$(fe show -tip root)
  $ fe change -set-base ${REV} root/test-feature \
  >     |& matches "locked.*(feature_path root/test-feature).*(lock_name Rebase)"
  [1]

Changing the human version of the base rev is authorized even when rebase is locked.

  $ REV=$(fe show -base root/test-feature)
  $ hg book nicer-tag-name -r $REV
  $ fe change -set-base nicer-tag-name root/test-feature

Rebase.

  $ fe unlock -rebase
  $ fe show -is-rebased root/test-feature
  false
  $ fe rebase root/test-feature >/dev/null
  $ fe show -is-rebased root/test-feature
  true
