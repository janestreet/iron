Slurp in the common setup code for the rebase tests:
  $ . $IRON_TEST_DIR/lib/test-rebase-preface.sh &>/dev/null

Regression test for a bug where fe reb returns the wrong error message
when the GCA is not unique.

Make our own revisions because we want a clean criss-cross merge and
that seems too special-purpose to push into the preface.

  $ hg pull -q "$remote_repo_dir"

-- Test: Give right error when greatest common ancestor is not unique:

  $ hg update -q -r 0
  $ echo c1 > f1.txt
  $ hg commit -q -m 'grandparent1'
  $ grandparent1=$(hg tip --template={node})
  $ hg update -q -r 0
  $ echo c2 > f1.txt
  $ hg commit -q -m 'grandparent2'
  $ grandparent2=$(hg tip --template={node})
  $ hg merge -q --tool 'internal:local' -r "$grandparent1"
  $ hg commit -q -m 'parent1'
  $ parent1=$(hg tip --template={node})
  $ hg update -q -r "$grandparent1"
  $ hg merge -q --tool 'internal:other' -r "$grandparent2"
  $ hg commit -q -m 'parent2'
  $ parent2=$(hg tip --template={node})
  $ hg push -q -f "$remote_repo_dir"
  $ (rb_diamond "$grandparent1" "$parent1" "$parent2" \
  >   -fake-valid-obligations -fake-valid-obligations || true) |&
  > fgrep -q "multiple greatest common ancestors"
