Start test. 

  $ start_test

Setup.

  $ mkdir repo
  $ cd repo
  $ hg init
  $ touch f1.txt
  $ hg add f1.txt
  $ hg commit -m "0"
  $ fe create root -description 'root' -remote-repo-path $(pwd)
  $ fe create root/a -description 'a'
  $ fe change -add-whole-feature-reviewers user1
  $ fe enable-review
  $ fe list root
  |------------------------------------|
  | feature |   lines | next step      |
  |---------+---------+----------------|
  | root    | pending | wait for hydra |
  |   a     | pending | wait for hydra |
  |------------------------------------|
  $ hg book | grep -q root
  $ hg book | grep -q root/a

Copy a feature.

  $ fe copy root root/b |& matches "feature has no parent"
  [1]
  $ fe copy root/a root/a/b |& matches "parents must be the same"
  [1]

  $ fe list root
  |------------------------------------|
  | feature |   lines | next step      |
  |---------+---------+----------------|
  | root    | pending | wait for hydra |
  |   a     | pending | wait for hydra |
  |------------------------------------|
  $ hg book | grep -q root
  $ hg book | grep -q root/a
  $ hg book | grep -q root/b
  [1]
  $ feature_to_server root/a -fake-valid
  $ fe copy root/a root/b
  $ feature_to_server root/b -fake-valid
  $ fe show root/a
  root/a
  ======
  a
  
  |---------------------------------------------------------|
  | attribute               | value                         |
  |-------------------------+-------------------------------|
  | next step               | add code                      |
  | owner                   | unix-login-for-testing        |
  | whole-feature reviewers | unix-login-for-testing, user1 |
  | seconder                | not seconded                  |
  | review is enabled       | true                          |
  | reviewing               | unix-login-for-testing        |
  | is permanent            | false                         |
  | tip                     | 9bc08b7747e4                  |
  | base                    | 9bc08b7747e4                  |
  |---------------------------------------------------------|
  $ fe show root/b
  root/b
  ======
  a
  
  |---------------------------------------------------------|
  | attribute               | value                         |
  |-------------------------+-------------------------------|
  | next step               | add code                      |
  | owner                   | unix-login-for-testing        |
  | whole-feature reviewers | unix-login-for-testing, user1 |
  | seconder                | not seconded                  |
  | review is enabled       | true                          |
  | reviewing               | unix-login-for-testing        |
  | is permanent            | false                         |
  | tip                     | 9bc08b7747e4                  |
  | base                    | 9bc08b7747e4                  |
  |---------------------------------------------------------|
  $ fe list root
  |------------------------------------|
  | feature |   lines | next step      |
  |---------+---------+----------------|
  | root    | pending | wait for hydra |
  |   a     |       0 | add code       |
  |   b     |       0 | add code       |
  |------------------------------------|
  $ hg book | grep -q root
  $ hg book | grep -q root/a
  $ hg book | grep -q root/b

Try to copy a feature with children.

  $ fe create root/a/c -description 'c'
  $ hg book | grep -q root/a/c
  $ fe list root/a
  |------------------------------------|
  | feature |   lines | next step      |
  |---------+---------+----------------|
  | root    |         |                |
  |   a     |       0 | add code       |
  |     c   | pending | wait for hydra |
  |------------------------------------|
  $ fe copy root/a root/d |& matches "feature has children"
  [1]
  $ fe list root
  |------------------------------------|
  | feature |   lines | next step      |
  |---------+---------+----------------|
  | root    | pending | wait for hydra |
  |   a     |       0 | add code       |
  |   b     |       0 | add code       |
  |------------------------------------|
  $ hg book | grep -q root/d
  [1]
  $ hg book | grep -q root/d/c
  [1]

Fail without empty brains or review sessions in progress.

  $ fe create root/feature -d root/feature
  $ touch b; hg add b; hg commit -m b
  $ fe enable
  $ fe change -add-reviewing file-owner
  $ feature_to_server root/feature -fake-valid
  $ fe session mark-file root/feature b
  $ IRON_USER=file-owner fe session mark-file root/feature b
  $ fe copy root/feature root/feature1 |& matches "review has been done"
  [1]
  $ fe copy root/feature root/feature1 -without-copying-review
  $ fe list root
  |-----------------------------------------|
  | feature    |   lines | next step        |
  |------------+---------+------------------|
  | root       | pending | wait for hydra   |
  |   a        |       0 | add code         |
  |   b        |       0 | add code         |
  |   feature  |       1 | add w-f-reviewer |
  |   feature1 | pending | wait for hydra   |
  |-----------------------------------------|
  $ hg book | grep -q root/feature1

Make the feature fully reviewed.

  $ feature_to_server root -fake-valid
  $ fe enable-review root
  $ fe second root -even-though-owner -even-though-empty
  $ fe second root/feature -even-though-owner
  $ fe show root/feature -next-step
  (Release)

Copying a fully reviewed feature preserves review state.

  $ fe copy root/feature root/fully-reviewed-copy -without-copying-review \
  >  |& matches "feature is fully reviewed"
  [1]
  $ fe copy root/feature root/fully-reviewed-copy
  $ feature_to_server root/fully-reviewed-copy -fake-valid
  $ fe second -even-though-owner
  $ fe show root/fully-reviewed-copy -next-step
  (Release)
