Start test.

  $ start_test

Make a repo, and add some features.

  $ mkdir repo
  $ cd repo
  $ hg init

  $ function add-file {
  >     num="$1"
  >     file=f$num.m1
  >     echo $num >$file
  >     hg add $file
  >     hg commit -q -m $num
  > }
  $ add-file 1
  $ rev1=$(hg log -r . --template {node})
  $ fe create root -description 'root' -remote-repo-path $(pwd)
  $ fe create root/a -description 'a'
  $ add-file 2
  $ feature_to_server root -fake-valid
  $ feature_to_server root/a -fake-valid
  $ fe tools pairwise-common-revisions -subtree root
  $ hg -q up root
  $ add-file 3
  $ feature_to_server root -fake-valid
  $ fe tools pairwise-common-revisions root root/a
  $ fe create root/b -description 'b'
  $ fe tools pairwise-common-revisions -subtree root
  $ add-file 4
  $ fe change root/b -set-base $rev1
  $ feature_to_server root/b -fake-valid
  $ fe tools pairwise-common-revisions root root/b \
  >     |& matches "common_revisions"
