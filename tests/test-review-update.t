Start test.

  $ start_test

Setup repo.

  $ setup_repo_and_root a
  $ feature_to_server root -fake-valid
  $ rev0=$(hg log -r . --template={node})
  $ echo change >a; hg commit -m 'change'
  $ rev1=$(hg log -r . --template={node})
  $ feature_to_server root -fake-valid
  $ fe enable

Helper functions.

  $ function create-local-clone {
  >     cd ..
  >     rm -rf local-clone
  >     hg clone -q repo local-clone "$@"
  >     cd local-clone
  > }

  $ function parent {
  >     hg parent --template={node}
  > }

  $ function parent-is {
  >   [ "$(parent)" = "$1" ]
  > }

  $ function review {
  >     IRON_OPTIONS='((workspaces false))'  \
  >       fe review root 2>&1 >/dev/null | grep -v -F 'Received EOF' || true
  > }

Can't infer the feature if the bookmark isn't there.

  $ create-local-clone
  $ hg book --delete root
  $ fe review |& matches 'could not determine feature you want to use'
  [1]

Review automatically pulls and updates to the bookmark.

  $ review
  $ hg book
   * root                      1:5727ade86dce

If the repo isn't clean, the review succeeds iff the bookmark is active.

  $ touch z
  $ review
  $ hg up -q -r $rev1
  $ review | matches 'needs to \[hg update\] but won'\''t.*hg repository is not clean'

Review pulls and updates to the necessary rev.

  $ create-local-clone -r $rev0
  $ hg log -r $rev1 |& matches 'unknown revision'
  [255]
  $ review
  $ hg log -r $rev1 >/dev/null
  $ hg book
   * root                      1:5727ade86dce

Review automatically updates if the bookmark is current but not active.

  $ create-local-clone
  $ hg book -f -r $rev0 root
  $ hg up -q -r root
  $ hg pull -q -r root
  $ cat .hg/bookmarks.current
  root (no-eol)
  $ hg active-bookmark
  [1]
  $ parent-is $rev0
  $ review
  $ parent-is $rev1

If the repo isn't clean, and Iron needs to pull
the error message includes the cleanliness error

  $ create-local-clone -r $rev0
  $ hg log -r $rev1 |& matches 'unknown revision'
  [255]
  $ touch z
  $ review | matches 'needs to pull but won'\''t.*hg repository is not clean'

