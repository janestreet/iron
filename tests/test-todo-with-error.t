  $ start_test

Creating a feature with some review, and a current session for the seconder:

  $ setup_repo_and_root a
  $ fe enable -add-whole-feature-reviewer seconder,user1
  $ fe change -set-reviewing-whole-feature-only
  $ echo aa > a; touch b; hg add b; hg commit -m b
  $ feature_to_server root -fake-valid
  $ fe session  mark-file root b -for seconder -reason reason
  $ fe catch-up mark-file root b -for seconder
  $ fe session show
  Reviewing root to e6cbcddb31ff.
  2 files to review: 3 lines total
     [ ] 2 a
     [ ] 1 b
  $ fe session show -for seconder
  Reviewing root to e6cbcddb31ff.
  1 files to review (1 already reviewed): 3 lines total
     [ ] 2 a
     [X] 1 b

And breaking the feature:

  $ echo '<<<<<<' > a; hg commit -m a
  $ feature_to_server root -fake-valid

And now, the owner sees the error. He also sees his existing session:

  $ fe todo
  |---------------------------------|
  | feature | review | next step    |
  |---------+--------+--------------|
  | root    |      3 | fix problems |
  |---------------------------------|
  
  Features you own:
  |--------------------------------------|
  | feature | tip | #left | next step    |
  |---------+-----+-------+--------------|
  | root    | <<< | error | fix problems |
  |--------------------------------------|

The seconder also sees his old session, but not the error:

  $ IRON_USER=seconder fe session show
  Reviewing root to e6cbcddb31ff.
  1 files to review (1 already reviewed): 3 lines total
     [ ] 2 a
     [X] 1 b

  $ IRON_USER=seconder fe todo
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |      2 |
  |------------------|

And user1 has neither review nor error in the todo:

  $ IRON_USER=user1 fe review \
  >     |& matches "cannot create review session -- the feature has problems that need to be fixed"
  [1]
  $ fe todo -for user1
