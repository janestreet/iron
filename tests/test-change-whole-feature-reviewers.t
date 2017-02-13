Start test.

  $ start_test

Create hg repo.

  $ mkdir repo
  $ cd repo
  $ hg init
  $ touch file
  $ hg add file
  $ hg com -m file
  $ remote=$(pwd)
  $ fe create root -owner owner -description root -remote-repo-path $remote
  $ echo >file
  $ hg com -m change
  $ feature_to_server root -fake-valid-obligations
  $ fe enable-review
  $ fe change -set-reviewing-all
  $ fe todo -for user1

Make user1 a whole-feature reviewer and he has stuff todo.

  $ fe change -add-whole-feature-reviewer user1
  $ fe todo -for user1
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |      1 |
  |------------------|

Make user1 not a whole-feature reviewer and he has nothing todo.

  $ fe change -remove-whole-feature-reviewer user1
  $ fe todo -for user1

Make user1 a whole-feature reviewer, create a session.

  $ fe change -add-whole-feature-reviewer user1
  $ IRON_USER=user1 fe session show >/dev/null 2>&1 # create a session
  $ SESSION_ID=$(fe session show -id -for user1)

Lock the session.

  $ IRON_USER=user1 fe session lock

Then remove them as a whole-feature reviewer.  Check that they get a warning.

  $ fe change -remove-whole-feature-reviewer user1
  $ fe session show -for user1
  Warning: what you need to review may have changed since this session was created.
  Consider committing or forgetting your current session:
  
  |-----------------------------------------------------|
  | reviewer                  | in session | in feature |
  |---------------------------+------------+------------|
  | is whole-feature reviewer | true       | false      |
  |-----------------------------------------------------|
  
  Warning: the feature has changed since this session was created.  It may be more suitable
  to review the feature to its most recent tip.  Consider committing your session:
  
  |-----------------------------------------------------------------|
  | remaining in session | session end to tip | remaining if commit |
  |----------------------+--------------------+---------------------|
  |                    1 |                  0 |                   0 |
  |-----------------------------------------------------------------|
  
  Reviewing root to 659c3710a572.
  1 files to review: 1 lines total
     [ ] 1 file

If user1 commits their session, then they have no new session and nothing to
review.

  $ fe session commit -session-id ${SESSION_ID} -for user1
  $ fe session diff -for user1 |& matches 'no current session'
  [1]

  $ fe show
  root
  ====
  root
  
  |---------------------------------------|
  | attribute              | value        |
  |------------------------+--------------|
  | next step              | review       |
  | owner                  | owner        |
  | whole-feature reviewer | owner        |
  | seconder               | not seconded |
  | review is enabled      | true         |
  | reviewing              | all          |
  | is permanent           | false        |
  | tip                    | 659c3710a572 |
  | base                   | 04da3968e088 |
  |---------------------------------------|
  
  |----------------|
  | user  | review |
  |-------+--------|
  | owner |      1 |
  |----------------|
