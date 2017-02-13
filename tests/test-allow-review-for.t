  $ start_test
  $ setup_sample_repo_and_root two-users
  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Users jdoe3 unix-login-for-testing)
  > (Allow_review_for (Users jdoe3)
  >                by All_users)
  > (Allow_review_for (Users unix-login-for-testing)
  >                by (Users user2))
  > (Allow_review_for (Users user1)
  >                by (Users unix-login-for-testing user2))
  > EOF
  $ hg add -q
  $ fe ob check
  $ hg commit -m init
  $ base=$(hg id | cut -d ' ' -f 1)
  $ fe change -set-base ${base}
  $ feature_to_server root
  $ fe create root/a -description 'root/a'
  $ seq 1 3 > a
  $ seq 1 5 > b
  $ hg addremove . -q
  $ hg commit -m "added some lines"
  $ feature_to_server root/a
  $ fe enable-review
  $ fe change -add-whole-feature-reviewers jdoe3
  $ fe change -set-reviewing-all
  $ fe show
  root/a
  ======
  root/a
  
  |---------------------------------------------------------|
  | attribute               | value                         |
  |-------------------------+-------------------------------|
  | next step               | review                        |
  | owner                   | unix-login-for-testing        |
  | whole-feature reviewers | jdoe3, unix-login-for-testing |
  | seconder                | not seconded                  |
  | review is enabled       | true                          |
  | reviewing               | all                           |
  | is permanent            | false                         |
  | tip                     | * | (glob)
  | base                    | * | (glob)
  |---------------------------------------------------------|
  
  |---------------------------------|
  | user                   | review |
  |------------------------+--------|
  | jdoe3                  |      8 |
  | unix-login-for-testing |      8 |
  | user2                  |      5 |
  | user1                  |      3 |
  |---------------------------------|

  $ fe show -allow-review-for
  (((reviewed_for (Users (user1)))
    (reviewed_by (Users (unix-login-for-testing user2))))
   ((reviewed_for (Users (unix-login-for-testing)))
    (reviewed_by (Users (user2))))
   ((reviewed_for (Users (jdoe3))) (reviewed_by All_users)))

One can also show the setting built off of the local state of the obligations
files.  This may be useful when working on some local change to that setting,
for example to verify a changeset before pushing it.

  $ fe obligations allow-review-for
  (((reviewed_for (Users (user1)))
    (reviewed_by (Users (unix-login-for-testing user2))))
   ((reviewed_for (Users (unix-login-for-testing)))
    (reviewed_by (Users (user2))))
   ((reviewed_for (Users (jdoe3))) (reviewed_by All_users)))

Now review for others.  We turn off display_ascii so that we begin
review (and potentially fail) rather than just printing the review
session.

  $ export IRON_OPTIONS='((display_ascii_always false))'
  $ echo "q" | fe review -for jdoe3 -reason testing | matches "Reviewing root/a"
  $ fe review -for user2 -reason testing |& matches "not allowed"
  [1]
  $ echo "q" | fe review -for user1 -reason testing \
  >   | matches "Reviewing root/a"
  $ echo "q" | fe review -for unix-login-for-testing -reason testing \
  >   | matches "Reviewing root/a"
  $ export IRON_OPTIONS='((display_ascii_always true))'

Sessions should be protected by the same permissions.

  $ fe session mark-file root/a a -for jdoe3 -reason test
  $ fe session mark-file root/a b -for user2 \
  >   |& matches "may-modify-others-review.*not allowed"
  [1]
  $ fe tools mark-fully-reviewed root/a -for all -reason test \
  >   |& matches "may-modify-others-review.*not allowed"
  [1]
  $ fe session show -for jdoe3 | matches "\[X\] 3 a"
  $ fe session show -for user2 | matches "\[ \] 5 b"
  $ fe session show -may-be-reviewed-by -for all
  jdoe3:
  All_users
  
  unix-login-for-testing:
  (Users (user2))
  
  user1:
  (Users (unix-login-for-testing user2))
  
  user2:
  (Users ())
  

  $ ID=$(fe session show -id -for user2)
  $ fe session forget -all -for user2 -session-id $ID \
  >   |& matches "modify-others-review.*not allowed"
  [1]
  $ fe session commit -for user2 -session-id $ID \
  >   |& matches "modify-others-review.*not allowed"
  [1]
  $ fe brain forget -for user2 -all \
  >   |& matches "modify-others-review.*not allowed"
  [1]

If we disable client-side checks the server should still not allow bad permissions.

  $ export IRON_FUNCTIONAL_TESTING_CLIENT_DOES_NOT_CHECK_REVIEW_PERMISSIONS=
  $ ID=$(fe session show -id -for user2)
  $ fe session forget -all -for user2 -session-id $ID |& matches "forget-session.*not allowed"
  [1]
  $ fe session commit -for user2 -session-id $ID |& matches "commit-session.*not allowed"
  [1]
  $ fe brain forget -for user2 -all |& matches "brain-forget.*not allowed"
  [1]
  $ unset IRON_FUNCTIONAL_TESTING_CLIENT_DOES_NOT_CHECK_REVIEW_PERMISSIONS

First we finish some review sessions.

  $ fe tools mark-fully-reviewed root/a -for user1 -reason test

Changing permissions.

  $ hg update root -q
  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Users jdoe3 unix-login-for-testing)
  > (Allow_review_for (Users user2)
  >                by (Users unix-login-for-testing))
  > EOF
  $ hg commit -m test -q
  $ base=$(hg id | cut -d ' ' -f 1)
  $ fe change -set-base ${base}
  $ feature_to_server root
  $ fe tools mark-fully-reviewed root -for all -reason test
  $ fe rebase root/a > /dev/null
  $ echo "a change" >> a
  $ echo "more lines" >> b
  $ hg commit -m test
  $ feature_to_server root/a

We can now review for user2, and can no longer start a new session for user1.
We can no longer review for jdoe3.

  $ export IRON_OPTIONS='((display_ascii_always false))'
  $ echo "q" | fe review -for user2 -reason testing | matches "Reviewing root/a"
  $ fe review -for user1 -reason testing -skip-catch-up-review \
  >   |& matches "modify-others-review.*not allowed"
  [1]
  $ echo "q" | fe review -for jdoe3 -reason testing -skip-catch-up-review \
  >   |& matches "modify-others-review.*not allowed"
  [1]
  $ export IRON_OPTIONS='((display_ascii_always true))'

Check persistence.

  $ fe-server stop
  $ fe-server start

  $ fe session show -may-be-reviewed-by -for all
  jdoe3:
  (Users ())
  
  unix-login-for-testing:
  (Users ())
  
  user1:
  (Users ())
  
  user2:
  (Users (unix-login-for-testing))
  
  $ export IRON_OPTIONS='((display_ascii_always false))'
  $ echo "q" | fe review -for user2  -reason testing | matches "Reviewing root/a"

  $ fe review -for user1 -reason testing -skip-catch-up-review \
  >   |& matches "modify-others-review.*not allowed"
  [1]

  $ export IRON_OPTIONS='((display_ascii_always true))'

Checking that when hydra mark everyones because of fully reviewed edges, we don't
step into permissions issues:

  $ fe show -next-step
  (Review)
  $ fe internal fully-reviewed-edge add -from $(fe show -base) -to $(fe show -tip)
  $ fe show -next-step
  (Review)
  $ feature_to_server root/a
  $ fe show -next-step
  (Ask_seconder)
