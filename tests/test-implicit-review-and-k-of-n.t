  $ start_test

Creating a feature where user1 and user2 have 1-of-2 review obligations in file1 and
file2.

  $ setup_repo_and_root file1 file2
  $ cat > .fe.sexp <<EOF
  > (Owner unix-login-for-testing)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > (Reviewed_by (At_least 1 of (Users user1 user2)))
  > (Apply_to (Files file1 file2))
  > EOF
  $ mkdir .fe
  $ touch .fe/obligations-repo.sexp
  $ cat > .fe/obligations-global.sexp <<EOF
  > (Define_scrutiny normal ((level 50) (description "normal")))
  > (Users unix-login-for-testing user1 user2)
  > EOF
  $ cat > .fe/.fe.sexp <<EOF
  > (Owner unix-login-for-testing)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF
  $ hg addremove -q
  $ hg commit -m 'base'
  $ fe change -set-base .
  $ feature_to_server root
  $ fe enable
  $ fe second -even-though-owner -even-though-empty
  $ fe change -set-whole-feature-reviewers ''

Then make user1 review all of the feature, so it becomes releasable:

  $ echo '1 1' > file1
  $ echo '2 2' > file2
  $ hg commit -m 'change'
  $ feature_to_server root
  $ IRON_USER=user1 fe tools mark-fully-reviewed root
  $ fe show -next-step
  (Release)
  $ fe show -omit-attribute-table
  root
  ====
  root
  
  |-------------------|
  | user  | completed |
  |-------+-----------|
  | user1 |         4 |
  |-------------------|

Now, when both an actual change is made and a whitespace change is made, fe asks user2 to
review the file with the whitespace change, even though user1 has already reviewed it
(which is wrong).

  $ echo '1  1' > file1
  $ echo 'change' > file2
  $ hg commit -m 'change'
  $ feature_to_server root
  $ fe show -next-step
  (Review)
  $ fe show -omit-attribute-table
  root
  ====
  root
  
  |----------------------------|
  | user  | review | completed |
  |-------+--------+-----------|
  | user2 |      4 |           |
  | user1 |      2 |         4 |
  |----------------------------|

But if user2 only reviews the actual change, the feature becomes releasable without
user1 needing to do anything:

  $ IRON_USER=user2 fe session mark-file root file2
  $ ( export IRON_USER=user2; fe session commit -session-id $(fe session show -id) )
  $ fe show -next-step
  (Release)
  $ fe show -omit-attribute-table
  root
  ====
  root
  
  |----------------------------|
  | user  | follow | completed |
  |-------+--------+-----------|
  | user1 |      2 |         4 |
  | user2 |        |         2 |
  |----------------------------|


