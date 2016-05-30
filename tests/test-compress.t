Start test.

  $ start_test

Setup.

  $ setup_repo_and_root file
  $ feature_to_server root -fake-valid
  $ r0=$(hg tip --template={node})
  $ echo change >file; hg com -m change file
  $ r1=$(hg tip --template={node})

Helper functions.

  $ function create_feature {
  >     fe create $1 -base $r0 -tip $r0 -description $1
  >     feature_to_server $1 -fake-valid
  > }

  $ function cleanup {
  >     for f in $(fe list -name-only -depth max | grep -vE 'root$' | sort -r); do
  >         fe archive $f
  >     done
  > }

Compressing a root feature is not allowed.

  $ fe compress root
  (error (prepare-to-compress (Failure "cannot compress a root feature")))
  [1]

Regardless of permanent status.

  $ fe change root -set-is-permanent false
  $ fe compress root
  (error (prepare-to-compress (Failure "cannot compress a root feature")))
  [1]
  $ fe change root -set-is-permanent true

Compressing a feature with no children is like archiving the feature.

  $ create_feature root/a
  $ fe list root -depth max
  |-----------------------------|
  | feature | lines | next step |
  |---------+-------+-----------|
  | root    |     0 | add code  |
  |   a     |     0 | add code  |
  |-----------------------------|
  $ fe change root/a -set-is-permanent true
  $ fe compress root/a
  (error (prepare-to-compress (Failure "cannot archive a permanent feature")))
  [1]
  $ fe change root/a -set-is-permanent false
  $ fe compress root/a
  $ fe list root -depth max
  |-----------------------------|
  | feature | lines | next step |
  |---------+-------+-----------|
  | root    |     0 | add code  |
  |-----------------------------|
  $ hg book | grep -qF root/a
  [1]

Compressing a nonempty feature is disallowed.

  $ create_feature root/a
  $ hg book root/a -f -r $r1
  $ feature_to_server root/a -fake-valid
  $ fe compress root/a |& matches "cannot compress a feature whose base and tip are unequal"
  [1]
  $ cleanup

Compressing is disallowed if it would overwrite an existing feature.

  $ create_feature root/a
  $ create_feature root/a/b
  $ create_feature root/b
  $ fe compress root/a |& matches "cannot rename to an existing feature"
  [1]
  $ cleanup

Compressing a feature with a child renames the child, and updates the active bookmark if
we were at a feature that was renamed.

  $ create_feature root/a
  $ create_feature root/a/b
  $ hg active-bookmark
  root/a/b
  $ fe list root -depth max
  |-----------------------------|
  | feature | lines | next step |
  |---------+-------+-----------|
  | root    |     0 | add code  |
  |   a     |     0 | add code  |
  |     b   |     0 | add code  |
  |-----------------------------|
  $ fe compress root/a
  $ fe list root -depth max
  |-----------------------------|
  | feature | lines | next step |
  |---------+-------+-----------|
  | root    |     0 | add code  |
  |   b     |     0 | add code  |
  |-----------------------------|
  $ hg book | grep -qF root/a
  [1]
  $ hg book | grep -qF root/a/b
  [1]
  $ hg book | grep -qF root/b
  $ hg active-bookmark
  root/b
  $ cleanup

Compressing a feature with a child whose basename is the same.

  $ create_feature root/a
  $ create_feature root/a/a
  $ id=$(fe show -id root/a/a)
  $ fe compress root/a
  $ [ $id = $(fe show -id root/a) ]
  $ hg book | grep -qF root/a
  $ hg book | grep -qF root/a/a
  [1]
  $ cleanup

Compressing a feature with multiple children, one of whose basename is the same.

  $ create_feature root/a
  $ create_feature root/a/a
  $ create_feature root/a/b
  $ create_feature root/a/c
  $ fe compress root/a
  $ fe list -depth max
  |-----------------------------|
  | feature | lines | next step |
  |---------+-------+-----------|
  | root    |     0 | add code  |
  |   a     |     0 | add code  |
  |   b     |     0 | add code  |
  |   c     |     0 | add code  |
  |-----------------------------|
  $ cleanup

Compressing a feature with grandchildren.

  $ create_feature root/a
  $ create_feature root/a/b
  $ create_feature root/a/b/c
  $ create_feature root/a/b/d
  $ fe compress root/a
  $ fe list -depth max
  |-----------------------------|
  | feature | lines | next step |
  |---------+-------+-----------|
  | root    |     0 | add code  |
  |   b     |     0 | add code  |
  |     c   |     0 | add code  |
  |     d   |     0 | add code  |
  |-----------------------------|
  $ fe compress root/b
  $ cleanup
