  $ start_test

  $ setup_repo_and_root a
  $ rev0=$(hg log -r . --template {node})
  $ echo b >a; hg com -m 1
  $ rev1=$(hg log -r . --template {node})
  $ attribute=$(cat <<EOF
  > ($rev0 ((file_reviewers (user2))))
  > ($rev1 ((file_reviewers (user2))))
  > EOF
  > )
  $ feature_to_server root -fake-valid -fake-attribute "$attribute"
  $ fe enable

Put the alias [user2], in my brain.

  $ fe tools mark-fully-reviewed root -for unix-login-for-testing

Update aliases and make a change.

  $ fe admin users update-valid-users-and-aliases -stdin <<EOF
  > ((username user1) (alias (user2)))
  > EOF
  $ echo 'c' >a; hg com -m 2
  $ rev2=$(hg log -r . --template {node})
  $ attribute=$(cat <<EOF
  > $attribute
  > ($rev2 ((file_reviewers (user2))))
  > EOF
  > )
  $ feature_to_server root -fake-valid -fake-attribute "$attribute"

Now, because of the attribute change, we need to forget the old diff and review the
diff from scratch.

  $ fe session diff -do-not-lock-session | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ a @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny level10
  old base d3873e73d99e | old tip 8de79e2f3ed3
  _
  | @@@@@@@@ Hunk 1/2 @@@@@@@@
  | @@@@@@@@ Forget this diff -- this file no longer has a diff you should know @@@@@@@@
  | @@@@@@@@ old base 1,2 old tip 1,2 @@@@@@@@
  | -|a
  | +|b
  |_
  base d3873e73d99e | tip b67cafba4fe2
  _
  | @@@@@@@@ Hunk 2/2 @@@@@@@@
  | @@@@@@@@ base 1,2 tip 1,2 @@@@@@@@
  | -|a
  | +|c
  |_

But if we de-alias, we only have to review the new diff.

  $ fe internal de-alias root
  ((de_aliased (unix-login-for-testing)) (nothing_to_do (user1 user2)))
  $ fe session diff -do-not-lock-session | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ a @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny level10
  base d3873e73d99e | old tip 8de79e2f3ed3 | new tip b67cafba4fe2
  @@@@@@@@ old tip 1,2 new tip 1,2 @@@@@@@@
  -|b
  +|c
