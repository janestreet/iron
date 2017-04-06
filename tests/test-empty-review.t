  $ start_test

Creating a feature with only whitespace to read

  $ setup_repo_and_root a
  $ feature_to_server root -fake-valid
  $ echo "a " > a; hg commit -m a
  $ feature_to_server root -fake-valid
  $ fe enable

And no review in the todo

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

Check that the owner does not get a 'second' in their todo if there is
another potential seconder who's not an owner.

  $ fe change -add-whole-feature-reviewers user1
  $ fe todo
  Features you own:
  |------------------------|
  | feature | next step    |
  |---------+--------------|
  | root    | ask seconder |
  |------------------------|

  $ fe todo -for user1
  |---------------------|
  | feature | next step |
  |---------+-----------|
  | root    | second    |
  |---------------------|

And no session either:

  $ fe session show |& matches "reviewer is up to date, no current session"
  [1]
