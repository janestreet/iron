Start test.

  $ start_test

  $ mkdir repo
  $ cd repo
  $ hg init
  $ touch file; hg add file; hg com -m 0
  $ r0=$(hg tip --template={node})
  $ remote=$(pwd)
  $ fe create root -description root -remote-repo-path $remote
  $ echo hello >file; hg com -m 1
  $ r1=$(hg tip --template={node})
  $ feature_to_server root -fake-valid

There is review to do.

  $ fe enable-review
  $ fe todo
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |      1 |
  |------------------|
  
  Features you own:
  |-----------------------------|
  | feature | #left | next step |
  |---------+-------+-----------|
  | root    |     1 | review    |
  |-----------------------------|

Mark, and then there is no review to do.

  $ fe session mark-file root file
  $ fe todo
  |----------------------------|
  | feature | next step        |
  |---------+------------------|
  | root    | add w-f-reviewer |
  |----------------------------|
  
  Features you own:
  |----------------------------|
  | feature | next step        |
  |---------+------------------|
  | root    | add w-f-reviewer |
  |----------------------------|

Make the feature empty, and there is review to do again, because we need to forget our
knowledge.

  $ fe change -set-base 1
  $ feature_to_server root -fake-valid
  $ fe todo
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |      1 |
  |------------------|
  
  Features you own:
  |-----------------------------|
  | feature | #left | next step |
  |---------+-------+-----------|
  | root    |     1 | review    |
  |-----------------------------|

Review, and then there is no subsequent review to do.

  $ fe session mark-file root file
  $ fe todo
  Features you own:
  |---------------------|
  | feature | next step |
  |---------+-----------|
  | root    | add code  |
  |---------------------|
