Start test. 

  $ start_test

Setup.

  $ mkdir repo
  $ cd repo
  $ hg init
  $ touch f1.txt
  $ hg add f1.txt
  $ hg commit -m "0"

Can't archive nonexistent feature.

  $ fe archive nonexistent-feature
  ("no such feature" nonexistent-feature)
  [1]

Create feature.

  $ fe create root -description 'root' -remote-repo-path $(pwd)
  $ echo "# $CR user1: do something" >f1.txt; hg com -m 'added CR' f1.txt
  $ feature_to_server root -fake-valid

The bookmark is there.

  $ hg log -r root >/dev/null

The feature is there.

  $ feature_id=$(fe show root -id)
  $ fe list -archived
  $ fe list
  |-----------------------------|
  | feature | lines | next step |
  |---------+-------+-----------|
  | root    |     1 | CRs       |
  |-----------------------------|
  $ fe show root
  root
  ====
  root
  
  |--------------------------------------------------|
  | attribute              | value                   |
  |------------------------+-------------------------|
  | next step              | CRs                     |
  | owner                  | unix-login-for-testing  |
  | whole-feature reviewer | unix-login-for-testing  |
  | seconder               | not seconded            |
  | review is enabled      | false                   |
  | CRs are enabled        | true                    |
  | reviewing              | whole-feature reviewers |
  | is permanent           | false                   |
  | tip                    | 570d1cbcb724            |
  |   tip is cr clean      | false                   |
  | base                   | 9bc08b7747e4            |
  |--------------------------------------------------|
  
  |--------------------------------------|
  | user                   | CRs | total |
  |------------------------+-----+-------|
  | unix-login-for-testing |   1 |     1 |
  | total                  |   1 |     1 |
  |--------------------------------------|
  
  |---------------------------------|
  | user                   | review |
  |------------------------+--------|
  | unix-login-for-testing |      1 |
  |---------------------------------|
  $ fe todo
  CRs and review line counts:
  |---------------|
  | feature | CRs |
  |---------+-----|
  | root    |   1 |
  |---------------|
  
  Features you own:
  |-----------------------------------|
  | feature | CRs | #left | next step |
  |---------+-----+-------+-----------|
  | root    |   1 |     1 | CRs       |
  |-----------------------------------|

Can't archive when not in the repo.

  $ cd /
  $ IRON_OPTIONS='((workspaces false))' fe archive root |& matches "inside an hg repo"
  [1]
  $ cd -
  $TESTTMP/repo

Can't archive a permanent feature.

  $ fe change -set-is-permanent true
  $ fe archive root |& matches "cannot archive a permanent feature"
  [1]
  $ fe change -set-is-permanent false

Can't archive a feature with children.

  $ fe create root/child -d '' -allow-non-cr-clean-base
  $ fe archive root
  (error
   (archive-feature (Failure "cannot archive a feature that has children")))
  [1]
  $ fe archive root/child

Archive it.

  $ fe archive root
  $ fe list
  $ fe list -archived -depth max
  |------------------------------------------------------------------------------|
  | feature | feature id                           | archived at                 |
  |---------+--------------------------------------+-----------------------------|
  | root    | * | * | (glob)
  |   child | * | * | (glob)
  |------------------------------------------------------------------------------|

The bookmark is gone.

  $ hg log -r root 2>/dev/null
  [255]

Can't archive again.

  $ fe archive root
  ("no such feature" root)
  [1]

Unarchive.

  $ hg book
  no bookmarks set
  $ fe unarchive root -id $feature_id
  $ hg book
   * root                      1:570d1cbcb724
  $ hg active-bookmark
  root
  $ fe list
  |-----------------------------|
  | feature | lines | next step |
  |---------+-------+-----------|
  | root    |     1 | CRs       |
  |-----------------------------|
  $ fe list -archived -depth max
  |------------------------------------------------------------------------------|
  | feature | feature id                           | archived at                 |
  |---------+--------------------------------------+-----------------------------|
  | root    |                                      |                             |
  |   child | * | * | (glob)
  |------------------------------------------------------------------------------|

  $ fe show root
  root
  ====
  root
  
  |--------------------------------------------------|
  | attribute              | value                   |
  |------------------------+-------------------------|
  | next step              | CRs                     |
  | owner                  | unix-login-for-testing  |
  | whole-feature reviewer | unix-login-for-testing  |
  | seconder               | not seconded            |
  | review is enabled      | false                   |
  | CRs are enabled        | true                    |
  | reviewing              | whole-feature reviewers |
  | is permanent           | false                   |
  | tip                    | 570d1cbcb724            |
  |   tip is cr clean      | false                   |
  | base                   | 9bc08b7747e4            |
  |--------------------------------------------------|
  
  |--------------------------------------|
  | user                   | CRs | total |
  |------------------------+-----+-------|
  | unix-login-for-testing |   1 |     1 |
  | total                  |   1 |     1 |
  |--------------------------------------|
  
  |---------------------------------|
  | user                   | review |
  |------------------------+--------|
  | unix-login-for-testing |      1 |
  |---------------------------------|
  $ fe todo
  CRs and review line counts:
  |---------------|
  | feature | CRs |
  |---------+-----|
  | root    |   1 |
  |---------------|
  
  Features you own:
  |-----------------------------------|
  | feature | CRs | #left | next step |
  |---------+-----+-------+-----------|
  | root    |   1 |     1 | CRs       |
  |-----------------------------------|

Change a review manager and then persist.

  $ fe enable
  $ fe session show
  Reviewing root to 570d1cbcb724.
  1 files to review: 1 lines total
     [ ] 1 f1.txt
  $ fe internal session mark-file root f1.txt
  $ fe todo
  CRs and review line counts:
  |---------------|
  | feature | CRs |
  |---------+-----|
  | root    |   1 |
  |---------------|
  
  Features you own:
  |---------------------------|
  | feature | CRs | next step |
  |---------+-----+-----------|
  | root    |   1 | CRs       |
  |---------------------------|
  $ fe-server stop
  $ fe-server start
  $ fe todo
  CRs and review line counts:
  |---------------|
  | feature | CRs |
  |---------+-----|
  | root    |   1 |
  |---------------|
  
  Features you own:
  |---------------------------|
  | feature | CRs | next step |
  |---------+-----+-----------|
  | root    |   1 | CRs       |
  |---------------------------|

Unarchiving waits until the archive is serialized.

  $ fe admin server serializer pause
  $ fe archive root
  $ ( sleep 1; fe admin server serializer resume ) &
  $ fe unarchive root -id $feature_id

Clean up root.

  $ hg up -qr root
  $ echo >f1.txt; hg com -m 'removed CR'
  $ feature_to_server root -fake-valid

Archiving features with funny names also works

  $ funny_name=root/foo-_.
  $ fe create $funny_name -d ''
  $ hg log -r 'bookmark()' --template '{bookmarks}\n'
  root root/foo-_.
  $ fe archive $funny_name
  $ hg log -r 'bookmark()' --template '{bookmarks}\n'
  root

Archiving a feature that you don't own

  $ feat=root/bar
  $ fe create $feat -d '' -owners owner
  $ hg log -r 'bookmark()' --template '{bookmarks}\n'
  root root/bar
  $ fe archive root/bar |& matches "only an owner of a feature can archive it"
  [1]
  $ fe archive root/bar -for owner
  $ hg log -r 'bookmark()' --template '{bookmarks}\n'
  root

Unarchiving a feature can't overwrite an existing feature.

  $ fe create root/z1 -d ''
  $ fe create root/z1/z2 -d ''
  $ id=$(fe show -id)
  $ fe archive root/z1/z2
  $ fe create root/z1/z2 -d ''
  $ fe unarchive root/z1/z2 -id $id |& matches "a feature with that name already exists"
  [1]

Check that unarchive does not need the id if there is no ambiguity.

  $ fe create root/blah -d ''
  $ fe archive root/blah
  $ fe unarchive root/blah

Check that unarchive gives an error message containing some useful context in
case of an ambiguity so that the user can resolve it.

  $ fe archive root/blah
  $ fe create root/blah -d ''
  $ fe archive root/blah

  $ fe unarchive root/blah
  (error
   (get-feature-maybe-archived
    ("multiple archived features matching"
     ((((feature_id *)) (glob)
       ((feature_path root/blah) (owners (unix-login-for-testing))
        (archived_at (*)))) (glob)
      (((feature_id *)) (glob)
       ((feature_path root/blah) (owners (unix-login-for-testing))
        (archived_at (*)))))))) (glob)
  [1]
