Start test.

  $ start_test

Setup repo.

  $ setup_repo_and_root .fe.sexp file other-file
  $ mkdir .fe
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Define_scrutiny normal  ((level 50) (description "normal")))
  > (Define_scrutiny ignore  ((level  0) (description "ignore")))
  > (Users unix-login-for-testing user1 user2)
  > EOF
  $ touch .fe/obligations-repo.sexp
  $ cat >.fe/.fe.sexp <<EOF
  > (Owner unix-login-for-testing)
  > (Scrutiny ignore)
  > (Apply_to All_files)
  > EOF
  $ cat >.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Reviewed_by (At_least 1 of_ (Users user1 user2)))
  > (Apply_to All_files)
  > EOF
  $ hg add -q .
  $ hg com -m 'added files'
  $ fe enable
  $ fe change -set-base root
  $ feature_to_server root
  $ fe ob check
  $ fe second -even-though-owner -even-though-empty

Create child, have user1 review it and release it.

  $ IRON_USER=user1 fe create root/child -d child
  $ fe enable
  $ echo first-change >file; hg com -m file
  $ feature_to_server root/child
  $ IRON_USER=user1 fe second -even-though-owner
  $ IRON_USER=user1 fe tools mark-fully-reviewed root/child
  $ fe release
  $ feature_to_server root

  $ fe show -omit-attribute-table -omit-description
  root
  ====
  
  |------------------------------------|
  | user                   | completed |
  |------------------------+-----------|
  | unix-login-for-testing |         2 |
  | user1                  |         2 |
  | user2                  |         2 |
  |------------------------------------|
  
  Included features:
    root/child

Simulate a rebase of root in which it would pick up conflicts.

  $ echo some-conflicts >file; hg com -m file
  $ feature_to_server root
  $ fe show -omit-attribute-table -omit-description
  root
  ====
  
  |---------------------------------------------|
  | user                   | review | completed |
  |------------------------+--------+-----------|
  | unix-login-for-testing |      2 |         2 |
  | user1                  |      2 |         2 |
  | user2                  |      2 |         2 |
  |---------------------------------------------|
  
  Included features:
    root/child

Have user2 review the conflicts.

  $ fe tools mark-fully-reviewed root
  $ IRON_USER=user2 fe tools mark-fully-reviewed root
  $ fe show -omit-attribute-table -omit-description
  root
  ====
  
  |------------------------------------|
  | user                   | completed |
  |------------------------+-----------|
  | unix-login-for-testing |         2 |
  | user1                  |         2 |
  | user2                  |         2 |
  |------------------------------------|
  
  Included features:
    root/child

  $ fe show -next-steps
  (Release)
  $ fe todo -for user1
  $ fe todo -for user2

Create child, have user1 review it and release it.

  $ IRON_USER=user1 fe create root/child -d child
  $ fe enable
  $ echo second-change >file; hg com -m file
  $ feature_to_server root/child
  $ IRON_USER=user1 fe second -even-though-owner
  $ IRON_USER=user1 fe tools mark-fully-reviewed root/child
  $ fe release
  $ feature_to_server root

  $ fe show -omit-attribute-table -omit-description
  root
  ====
  
  |------------------------------------|
  | user                   | completed |
  |------------------------+-----------|
  | unix-login-for-testing |         2 |
  | user1                  |         2 |
  | user2                  |         2 |
  |------------------------------------|
  
  Included features:
    root/child
    root/child

Simulate another rebase of root in which it would pick up conflicts again.
This time, user1 will review the conflicts.

  $ echo more-conflicts >file; hg com -m file
  $ feature_to_server root
  $ fe show -omit-attribute-table -omit-description
  root
  ====
  
  |---------------------------------------------|
  | user                   | review | completed |
  |------------------------+--------+-----------|
  | unix-login-for-testing |      2 |         2 |
  | user1                  |      2 |         2 |
  | user2                  |      2 |         2 |
  |---------------------------------------------|
  
  Included features:
    root/child
    root/child

Have user1 review the conflicts.  Check that no spurious follow lines are
generated for user2.

  $ fe tools mark-fully-reviewed root
  $ IRON_USER=user1 fe tools mark-fully-reviewed root
  $ fe show -omit-attribute-table -omit-description
  root
  ====
  
  |------------------------------------|
  | user                   | completed |
  |------------------------+-----------|
  | unix-login-for-testing |         2 |
  | user1                  |         2 |
  | user2                  |         2 |
  |------------------------------------|
  
  Included features:
    root/child
    root/child

  $ fe show -next-steps
  (Release)
  $ fe todo -for user1
  $ fe todo -for user2
