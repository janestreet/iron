Start test.

  $ start_test

Setup.

  $ hg init remote
  $ cd remote
  $ remote_repo_path=$(pwd)
  $ touch f1.txt; hg add f1.txt; hg com -m f1.txt
  $ cd ..
  $ hg clone -q remote root
  $ cd root
  $ ROOT_DIR=$PWD
  $ fe create root  -d root  -permanent -remote-repo-path "$remote_repo_path"
  $ cd ..
  $ hg clone -q remote root2
  $ cd root2
  $ ROOT2_DIR=$PWD
  $ fe create root2 -d root2 -remote-repo-path "$remote_repo_path"
  $ cd ..
  $ cd root
  $ fe list
  |------------------------------------|
  | feature |   lines | next step      |
  |---------+---------+----------------|
  | root    | pending | wait for hydra |
  | root2   | pending | wait for hydra |
  |------------------------------------|

Make a sub feature.

  $ fe create root/foo  -description 'foo'
  $ fe todo
  Features you own:
  |----------------------------------------------|
  | feature | base    | tip     | next step      |
  |---------+---------+---------+----------------|
  | root    | pending | pending | wait for hydra |
  |   foo   | pending | pending | wait for hydra |
  | root2   | pending | pending | wait for hydra |
  |----------------------------------------------|

Failing renames.

  $ fe rename root/no root/no2
  ("no such feature" root/no)
  [1]
  $ feature_to_server root/foo -fake-valid
  $ fe create root/par -description 'par' -permanent
  $ fe todo
  Features you own:
  |----------------------------------------------|
  | feature | base    | tip     | next step      |
  |---------+---------+---------+----------------|
  | root    | pending | pending | wait for hydra |
  |   foo   |         |         | add code       |
  |   par   | pending | pending | wait for hydra |
  | root2   | pending | pending | wait for hydra |
  |----------------------------------------------|
  $ fe rename root root |& matches "cannot rename a root feature"
  [1]
  $ fe rename root root2 |& matches "cannot rename a root feature"
  [1]
  $ fe rename root/foo root2/foo |& matches "features must have the same root"
  [1]
  $ fe rename root new-root/child |& matches "cannot rename a root feature"
  [1]
  $ fe rename root/foo new-root |& matches "cannot rename to a root feature"
  [1]
  $ fe rename root/foo root/foo |& matches "cannot rename a feature to a descendant of itself"
  [1]
  $ fe rename root/foo root/foo/bar \
  >     |& matches "cannot rename a feature to a descendant of itself"
  [1]
  $ fe rename root/foo root/par |& matches "cannot rename to an existing feature"
  [1]
  $ fe rename root/foo root/no-such-feature/foo \
  >     |& matches "the parent of the target feature has to exist"
  [1]
  $ ( cd $ROOT2_DIR && fe archive root2 )

Rename root/foo and check feature by owners work.

  $ fe lock root/foo -rename -reason test
  $ fe rename root/foo root/bar \
  >     |& matches "feature lock is locked.*(feature_path root/foo) (lock_name Rename).*"
  [1]
  $ fe unlock root/foo -rename
  $ fe rename root/foo root/bar
  $ fe todo
  Features you own:
  |----------------------------------------------|
  | feature | base    | tip     | next step      |
  |---------+---------+---------+----------------|
  | root    | pending | pending | wait for hydra |
  |   bar   |         |         | add code       |
  |   par   | pending | pending | wait for hydra |
  |----------------------------------------------|
  $ fe list root
  |------------------------------------|
  | feature |   lines | next step      |
  |---------+---------+----------------|
  | root    | pending | wait for hydra |
  |   bar   |       0 | add code       |
  |   par   | pending | wait for hydra |
  |------------------------------------|
  $ hg book -q
  root
  root/bar
  root/par
  $ hg --cwd ../remote book -q
  root
  root/bar
  root/par

Rename non-leaf.

  $ fe create root/bar/a   -description 'a'
  $ fe create root/bar/b   -description 'b'
  $ fe create root/bar/b/c -description 'c'
  $ fe list root/bar -depth max
  |------------------------------------|
  | feature |   lines | next step      |
  |---------+---------+----------------|
  | root    |         |                |
  |   bar   |       0 | add code       |
  |     a   | pending | wait for hydra |
  |     b   | pending | wait for hydra |
  |       c | pending | wait for hydra |
  |------------------------------------|
  $ feature_to_server root/bar/a -fake-valid
  $ feature_to_server root/bar/b -fake-valid
  $ feature_to_server root/bar/b/c -fake-valid

Now, we create three situations:
1. missing remote bookmark (to be fixed manually)
2. missing local bookmark
3. diverging bookmarks (to be fixed manually)

  $ hg --cwd ../remote book --delete root/bar/b
  $ hg book --delete root/bar/a
  $ hg -q up -r null; echo a > a; hg add a; hg -q commit -m a; hg book -f root/bar/b/c
  $ fe rename root/bar root/foo |& matches "unknown revision 'root/bar/b'"
  [1]
  $ hg --cwd  ../remote book root/bar/b -r $(fe show root/bar/b -tip)
  $ fe rename root/bar root/foo |& matches "cannot rename with conflicting bookmarks.*root/bar/b/c"
  [1]
  $ hg book -d root/bar/b/c root/bar/b/c@default
  $ fe rename root/bar root/foo
  $ fe list root/foo -depth max
  |-----------------------------|
  | feature | lines | next step |
  |---------+-------+-----------|
  | root    |       |           |
  |   foo   |     0 | add code  |
  |     a   |     0 | add code  |
  |     b   |     0 | add code  |
  |       c |     0 | add code  |
  |-----------------------------|
  $ hg book -q
  root
  root/foo
  root/foo/a
  root/foo/b
  root/foo/b/c
  root/par
  $ hg --cwd ../remote book -q
  root
  root/foo
  root/foo/a
  root/foo/b
  root/foo/b/c
  root/par

Review managers test.

  $ fe create root/baz -description 'foo' -owner owner
  $ touch f2.txt
  $ hg add f2.txt
  $ hg commit -m "0"
  $ fe enable-review -add-whole-feature-reviewer user1,user2
  $ fe change -set-reviewing-whole-feature-only
  $ feature_to_server root/baz -fake-valid
  $ fe tools mark-fully-reviewed root/baz -for user2 -reason reason
  $ fe todo -for user1
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |        |
  |   baz   |      1 |
  |------------------|
  $ fe todo -for user2
  |--------------------|
  | feature | catch-up |
  |---------+----------|
  | root    |          |
  |   baz   |        1 |
  |--------------------|
  $ fe show
  root/baz
  ========
  foo
  
  |---------------------------------------------------|
  | attribute               | value                   |
  |-------------------------+-------------------------|
  | next step               | review                  |
  | owner                   | owner                   |
  | whole-feature reviewers | owner, user1, user2     |
  | seconder                | not seconded            |
  | review is enabled       | true                    |
  | reviewing               | whole-feature reviewers |
  | is permanent            | false                   |
  | tip                     | e26136c25e5a            |
  | base                    | 2673d446e419            |
  |---------------------------------------------------|
  
  |--------------------------------------------|
  | user       | review | catch-up | completed |
  |------------+--------+----------+-----------|
  | file-owner |      1 |          |           |
  | owner      |      1 |          |           |
  | user1      |      1 |          |           |
  | user2      |        |        1 |         1 |
  |--------------------------------------------|

  $ fe rename root/baz root/bzz
  $ fe todo -for user1
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |        |
  |   bzz   |      1 |
  |------------------|
  $ fe todo -for user2
  |--------------------|
  | feature | catch-up |
  |---------+----------|
  | root    |          |
  |   bzz   |        1 |
  |--------------------|
  $ fe show
  root/bzz
  ========
  foo
  
  |---------------------------------------------------|
  | attribute               | value                   |
  |-------------------------+-------------------------|
  | next step               | review                  |
  | owner                   | owner                   |
  | whole-feature reviewers | owner, user1, user2     |
  | seconder                | not seconded            |
  | review is enabled       | true                    |
  | reviewing               | whole-feature reviewers |
  | is permanent            | false                   |
  | tip                     | e26136c25e5a            |
  | base                    | 2673d446e419            |
  |---------------------------------------------------|
  
  |--------------------------------------------|
  | user       | review | catch-up | completed |
  |------------+--------+----------+-----------|
  | file-owner |      1 |          |           |
  | owner      |      1 |          |           |
  | user1      |      1 |          |           |
  | user2      |        |        1 |         1 |
  |--------------------------------------------|

Check consistency after restart.

  $ fe-server stop
  $ fe-server start

  $ fe todo -for user1
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |        |
  |   bzz   |      1 |
  |------------------|
  $ fe todo -for user2
  |--------------------|
  | feature | catch-up |
  |---------+----------|
  | root    |          |
  |   bzz   |        1 |
  |--------------------|
  $ fe show
  root/bzz
  ========
  foo
  
  |---------------------------------------------------|
  | attribute               | value                   |
  |-------------------------+-------------------------|
  | next step               | review                  |
  | owner                   | owner                   |
  | whole-feature reviewers | owner, user1, user2     |
  | seconder                | not seconded            |
  | review is enabled       | true                    |
  | reviewing               | whole-feature reviewers |
  | is permanent            | false                   |
  | tip                     | e26136c25e5a            |
  | base                    | 2673d446e419            |
  |---------------------------------------------------|
  
  |--------------------------------------------|
  | user       | review | catch-up | completed |
  |------------+--------+----------+-----------|
  | file-owner |      1 |          |           |
  | owner      |      1 |          |           |
  | user1      |      1 |          |           |
  | user2      |        |        1 |         1 |
  |--------------------------------------------|

Move a feature into a new parent, keep it's basename.

  $ fe create root/keep-this-basename -description 'd'
  $ fe create root/child              -description 'd'
  $ fe list root -depth max -name-only
  root
  root/bzz
  root/child
  root/foo
  root/foo/a
  root/foo/b
  root/foo/b/c
  root/keep-this-basename
  root/par

  $ fe rename root/keep-this-basename -new-parent root/child
  $ fe list root -depth max -name-only
  root
  root/bzz
  root/child
  root/child/keep-this-basename
  root/foo
  root/foo/a
  root/foo/b
  root/foo/b/c
  root/par
