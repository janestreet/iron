Start test.

  $ start_test

Setup repo.

  $ setup_repo_and_root .fe/.fe.sexp
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Define_scrutiny normal ((level 50) (description "normal")))
  > (Users unix-login-for-testing user1 user2)
  > EOF
  $ touch .fe/obligations-repo.sexp
  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF
  $ touch .
  $ hg add -q .
  $ hg com -m 'added files'
  $ fe change -set-base $(hg tip --template={node})

Change the owner from [user1] to [user2].

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user2)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF
  $ hg com -m 'changed owner'
  $ feature_to_server root

The w-f-reviewer has review to do.  The users involved in ownership changes are
not required to review to release.

  $ fe show -omit-attribute-table
  root
  ====
  root
  
  |---------------------------------|
  | user                   | review |
  |------------------------+--------|
  | unix-login-for-testing |      5 |
  | user1                  |      5 |
  | user2                  |      5 |
  |---------------------------------|
  $ fe session show -for user1
  Reviewing root to 0fa707fdc248.
  3 files to review: 5 lines total
  
  Ownership changes.
     [ ] 3 .fe/.fe.sexp
     [ ] 1 .fe/obligations-global.sexp
     [ ] 1 .fe/obligations-repo.sexp
  $ fe session show -for user2
  Reviewing root to 0fa707fdc248.
  3 files to review: 5 lines total
  
  Ownership changes.
     [ ] 3 .fe/.fe.sexp
     [ ] 1 .fe/obligations-global.sexp
     [ ] 1 .fe/obligations-repo.sexp

  $ fe enable-review
  $ fe tools mark-fully-reviewed root
  $ fe second -even-though-owner

  $ fe show -omit-attribute-table
  root
  ====
  root
  
  |---------------------------------------------|
  | user                   | review | completed |
  |------------------------+--------+-----------|
  | user1                  |      5 |           |
  | user2                  |      5 |           |
  | unix-login-for-testing |        |         5 |
  |---------------------------------------------|

  $ fe show -next-steps
  (Release)

  $ fe release

  $ fe todo -for user1
