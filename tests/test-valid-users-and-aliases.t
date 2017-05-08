------------------ Start test.-------------------------------

  $ start_test

-------------------- Create the users cache. ------------------------

Get user names from users.

  $ fe admin users update-valid-users-and-aliases -stdin <<EOF
  > ((username jdoe) (alias (johnny)))
  > ((username jdoe3))
  > ((username bfranklin))
  > ((username gwashington))
  > ((username jmonroe) (alias (jim monroe)))
  > ((username jdoe4))
  > ((username unix-login-for-testing))
  > EOF

  $ fe internal dump user-info valid-users
  (bfranklin gwashington jdoe jdoe3 jdoe4 jmonroe unix-login-for-testing)

  $ fe internal dump user-info aliases
  ((jim jmonroe) (johnny jdoe) (monroe jmonroe))

---------------- Test refreshing user names ------------------

Refresh users cache, by adding ahamilton and user1 and removing monroe.

  $ fe admin users update-valid-users-and-aliases -stdin <<EOF
  > ((username jdoe) (alias (johnny)))
  > ((username jdoe3))
  > ((username bfranklin))
  > ((username ahamilton) (alias (alex)))
  > ((username jdoe4))
  > ((username user1) (alias (user2)))
  > ((username unix-login-for-testing))
  > EOF
  $ fe admin users refresh-existing-users

Dump only the _current_ valid user names.

  $ fe internal dump user-info valid-users
  (ahamilton bfranklin jdoe jdoe3 jdoe4 unix-login-for-testing user1)

Dump _accumulated_ aliases.

  $ fe internal dump user-info aliases
  ((alex ahamilton) (jim jmonroe) (johnny jdoe) (monroe jmonroe) (user2 user1))

-------------- Try to add a current login who is an alias --------

  $ fe admin users update-valid-users-and-aliases -stdin <<< "((username johnny))" \
  >     |& matches "collisions in the user info.*((johnny ((Alias_of jdoe) Valid_user)))"
  [1]

------------ Try to add a current login who is a typo --------

  $ fe admin users define-typos -typo dcu -means jdoe
  $ fe admin users update-valid-users-and-aliases -stdin <<< "((username dcu))" \
  >     |& matches "collisions in the user info.*((dcu ((Typo_of jdoe) Valid_user))"
  [1]

---------- Try to add an alias who is a typo -----

  $ fe admin users update-valid-users-and-aliases -stdin <<EOF |&
  > ((username jdoe) (alias (dcu)))
  > EOF
  > matches "collisions in the user info.*((dcu ((Typo_of jdoe) (Alias_of jdoe)"
  [1]

--------- Try to add the same alias twice --------

  $ fe admin users update-valid-users-and-aliases -stdin <<EOF |&
  > ((username johnnycui) (alias (johnny)))
  > EOF
  > matches "alias can be resolved to different user names.*johnny.*johnnycui jdoe"
  [1]

--------------- Test persistence -----------------------

  $ fe-server stop
  $ fe-server start
  $ fe internal dump user-info valid-users
  (ahamilton bfranklin jdoe jdoe3 jdoe4 unix-login-for-testing user1)

  $ fe internal dump user-info aliases
  ((alex ahamilton) (jim jmonroe) (johnny jdoe) (monroe jmonroe) (user2 user1))

-------------- Remove an alias ------------------------

  $ fe admin users remove-aliases alex foobar \
  >     |& matches "The following aliases were not removed:.*foobar"
  [1]
  $ fe internal dump user-info aliases
  ((jim jmonroe) (johnny jdoe) (monroe jmonroe) (user2 user1))

-------------- Create hg repo ------------------------

  $ copy_sample_repo two-users ./repo
  $ cd repo
  $ hg init
  $ remote="$PWD"
  $ hg add -q .
  $ hg commit -m "init"
  $ fe create root -description "root" -remote-repo-path $remote
  $ cat > b <<EOF
  > (* $CR jdoe3 for johnny: 1/1 *)
  > (* $CR-soon jdoe3 for johnny: 1/1 *)
  > EOF
  $ cat > a <<EOF
  > (* $CR user1 for user2: 1/1 *)
  > (* $CR user2 for user1: 1/1 *)
  > EOF
  $ hg commit -m "add crs"
  $ feature_to_server root

---------- Checking that [fe show] also doesn't show aliases ---------

  $ fe show
  root
  ====
  root
  
  |-------------------------------------------------|
  | attribute              | value                  |
  |------------------------+------------------------|
  | next step              | CRs                    |
  | owner                  | unix-login-for-testing |
  | whole-feature reviewer | unix-login-for-testing |
  | seconder               | not seconded           |
  | review is enabled      | false                  |
  | CRs are enabled        | true                   |
  | reviewing              | unix-login-for-testing |
  | is permanent           | false                  |
  | tip                    | 0ff569637015           |
  |   tip is cr clean      | false                  |
  | base                   | 0f5b3abd13b6           |
  |-------------------------------------------------|
  
  |---------------------|
  | user  | CRs | total |
  |-------+-----+-------|
  | user1 |   2 |     2 |
  | jdoe  |   1 |     1 |
  | total |   3 |     3 |
  |---------------------|
  
  |---------------------------------|
  | user                   | review |
  |------------------------+--------|
  | unix-login-for-testing |      4 |
  | user1                  |      4 |
  |---------------------------------|

-------------- Test obligations --------------

  $ fe enable-review root
  $ fe change root -set-reviewing 'user1,user2' |& matches "\"never heard of users\" (user2)"
  [1]
  $ fe change root -set-reviewing 'user1'
  $ fe todo -for user1
  |------------------------|
  | feature | CRs | review |
  |---------+-----+--------|
  | root    |   2 |      4 |
  |------------------------|
