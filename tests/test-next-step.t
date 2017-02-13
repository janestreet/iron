Start test.

  $ start_test

Create repo

  $ mkdir repo
  $ cd repo
  $ hg init
  $ touch f1.ml
  $ hg add f1.ml
  $ hg commit -m "0"

Create some features

  $ fe create root -description 'root' -remote-repo-path $(pwd)
  $ echo change >f1.ml; hg com -m change
  $ feature_to_server root -fake-valid
  $ fe create root/a -description 'a'

Test various next steps

  $ fe list -depth max
  |------------------------------------|
  | feature |   lines | next step      |
  |---------+---------+----------------|
  | root    |       1 | enable-review  |
  |   a     | pending | wait for hydra |
  |------------------------------------|
  $ feature_to_server root/a -fake-valid
  $ fe list -depth max
  |---------------------------------|
  | feature | lines | next step     |
  |---------+-------+---------------|
  | root    |     1 | enable-review |
  |   a     |     0 | add code      |
  |---------------------------------|
  $ fe show -next-step
  (Add_code)
  $ cat > f2.ml <<EOF
  >   (* $CR user1: cr bla *)
  > EOF
  $ hg add f2.ml
  $ hg commit -m "1"
  $ feature_to_server root/a -fake-valid
  $ fe show -next-step
  (CRs)
  $ fe todo
  |---------------|
  | feature | CRs |
  |---------+-----|
  | root    |     |
  |   a     |   1 |
  |---------------|
  
  Features you own:
  |---------------------------------------|
  | feature | CRs | #left | next step     |
  |---------+-----+-------+---------------|
  | root    |     |     1 | enable-review |
  |   a     |   1 |     2 | CRs           |
  |---------------------------------------|
  $ echo "review me!" > f2.ml
  $ hg commit -m "2"
  $ feature_to_server root/a -fake-valid
  $ fe show -next-step
  (Enable_review)
  $ fe enable-review root/a
  $ fe show -next-step
  (Review)
  $ cat > f2.ml <<EOF
  >   (* $CR user1: cr bla *)
  > EOF
  $ hg commit -m "3"
  $ feature_to_server root/a -fake-valid
  $ fe show -next-step
  (CRs Review)
  $ fe disable-review root/a
  $ fe show -next-step
  (CRs)
  $ fe enable-review root/a
  $ fe change root/a -add-reviewing file-owner
  $ fe list -depth max
  |---------------------------------|
  | feature | lines | next step     |
  |---------+-------+---------------|
  | root    |     1 | enable-review |
  |   a     |     2 | CRs, review   |
  |---------------------------------|
  $ echo "review me!" > f2.ml
  $ hg commit -m "4"
  $ feature_to_server root/a -fake-valid
  $ fe session mark-file root/a f2.ml
  $ IRON_USER=file-owner fe session mark-file root/a f2.ml
  $ fe show -next-step
  (Add_whole_feature_reviewer)
  $ fe change -add-whole-feature-reviewers user1
  $ fe change -set-reviewing-none
  $ fe show -next-step
  (Widen_reviewing)
  $ fe change -set-reviewing-all
  $ fe session mark-file root/a f2.ml -for user1 -reason reason
  $ fe show -next-step
  (Ask_seconder)
  $ IRON_USER=user1 fe second
  $ fe show -next-step
  ((In_parent Enable_review))
  $ fe enable root
  $ fe second root -even-though-owner
  $ fe show -next-step
  ((In_parent Review))
  $ fe session mark-file root f1.ml
  $ fe show -next-step
  (Release)
  $ hg -q up root
  $ echo "rebase to me!" > f1.ml
  $ hg -q commit -m "5"
  $ feature_to_server root -fake-valid
  $ fe enable-review root
  $ fe show root/a -next-step
  (Rebase (In_parent Review))
  $ fe session mark-file root f1.ml
  $ hg -q up root/a
  $ fe list -depth max
  |-----------------------------------|
  | feature | lines | next step       |
  |---------+-------+-----------------|
  | root    |     1 | release         |
  |   a     |     2 | rebase, release |
  |-----------------------------------|
  $ fe show -next-step
  (Rebase Release)
  $ fe rebase
  1 files updated, 0 files merged, 0 files removed, 0 files unresolved
  (branch merge, don't forget to commit)
  $ fe show -next-step
  (Wait_for_hydra)
  $ feature_to_server root/a -fake-valid
  $ fe show -next-step
  (Release)
