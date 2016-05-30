  $ start_test

Add some users.

  $ fe admin users update-valid-users-and-aliases -stdin <<EOF
  > ((username bfranklin) (alias (ben)))
  > ((username jdoe4))
  > ((username unix-login-for-testing))
  > EOF
  $ fe admin users refresh-existing-users

When there is no work, there is nothing to complain about:

  $ fe admin users get-invalid

Setup a repository with a few crs.

  $ setup_repo_and_root file1.ml
  $ cat > file1.ml <<EOF
  > (* $CR jdoe4 for invalid-user: cr3 *)
  > (* $CR jdoe4 for ben: cr1 *)
  > (* $CR jdoe4 for jdoe4: cr2 *)
  > EOF
  $ hg commit -m "add crs"
  $ feature_to_server root -fake-valid-obligations
  $ fe change root -add-whole-feature-reviewers jdoe4

Act as though [jdoe4] and [unix-login-for-testing] have left and refresh users.

  $ fe admin users update-valid-users-and-aliases -stdin <<EOF
  > ((username bfranklin) (alias (ben)))
  > EOF
  $ fe admin users refresh-existing-users

[invalid-user], [unix-login-for-testing] and [jdoe4] are now invalid
users.  [ben] is not because it's not a user (it's an alias).

  $ fe admin users get-invalid
  |------------------------------------------------------------------------|
  | invalid user           | occurs as                                     |
  |------------------------+-----------------------------------------------|
  | invalid-user           | cr assignee                                   |
  | jdoe4                  | cr assignee, reviewer, whole-feature reviewer |
  | unix-login-for-testing | owner, reviewer, whole-feature reviewer       |
  |------------------------------------------------------------------------|
