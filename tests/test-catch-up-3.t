Start test.

  $ start_test

Setup a repo with two files:

  $ setup_repo_and_root a b
  $ echo "hello a" > a
  $ echo "hello b" > b
  $ hg commit -m "commit"
  $ feature_to_server root -fake-valid-obligations

  $ function show-catch-up {
  >     fe show -omit-review-sessions-in-progress-table \
  >       | sed -rn '/\| user .*\|/,$ p'
  > }

Enable.

  $ fe enable-review root -add-whole-feature-reviewers user1,user2
  $ fe change root -set-reviewing-all

Review & Catch up.

  $ fe internal session show-num-lines root  -for user1
  2
  $ fe internal catch-up show-num-lines root -for user1
  0
  $ fe session mark-file root a     -for user1 -reason reason
  $ fe internal session show-num-lines root  -for user1
  1
  $ fe internal catch-up show-num-lines root -for user1
  1
  $ fe session mark-file root a     -for user2 -reason reason
  $ fe internal session show-num-lines root  -for user2
  1
  $ fe internal catch-up show-num-lines root -for user2
  1
  $ show-catch-up
  | user                   | review | catch-up | completed |
  |------------------------+--------+----------+-----------|
  | unix-login-for-testing |      2 |          |           |
  | user1                  |      1 |        1 |         1 |
  | user2                  |      1 |        1 |         1 |
  |--------------------------------------------------------|

  $ fe-server stop
  $ export IRON_FUNCTIONAL_TESTING_CATCH_UP=
  $ fe-server start

Check that the env var above stops the ability to catch up on behalf
of someone else even though in test mode.

  $ fe review -only-catch-up-review -for user2 > /dev/null
  (error
   (may-modify-others-catch-up
    ("unauthorized attempt to modify someone else's catch-up review"
     ((for_ user2) (requested_by unix-login-for-testing)))))
  [1]

  $ fe catch-up clear -for user2
  (error
   (clear-catch-up-sessions
    ("unauthorized attempt to modify someone else's catch-up review"
     ((for_ user2) (requested_by unix-login-for-testing)))))
  [1]

  $ fe catch-up mark-file root a -for user2
  (error
   (catch-up-diffs
    ("unauthorized attempt to modify someone else's catch-up review"
     ((for_ user2) (requested_by unix-login-for-testing)))))
  [1]

Replace user_info.
user2 is now an invalid user, and user1 is an alias for unix-login-for-testing

  $ fe admin users update-valid-users-and-aliases -stdin <<EOF
  > ((username unix-login-for-testing) (alias (user1)))
  > EOF

Allow to anyone via -for to review invalid users catch-up sessions

  $ fe internal catch-up show-num-lines root -for user2
  1
  $ fe catch-up mark-file root a -for user2
  $ fe internal catch-up show-num-lines root -for user2
  0
  $ show-catch-up
  | user                   | review | catch-up | completed |
  |------------------------+--------+----------+-----------|
  | unix-login-for-testing |      2 |          |           |
  | user1                  |      1 |        1 |         1 |
  | user2                  |      1 |          |         1 |
  |--------------------------------------------------------|

Do not generate catch-up when reviewing for an invalid user

  $ fe internal session show-num-lines root  -for user2
  1
  $ fe internal catch-up show-num-lines root -for user2
  0
  $ fe session mark-file root b -for user2 -reason reason
  $ fe internal session show-num-lines root  -for user2
  0
  $ fe internal catch-up show-num-lines root -for user2
  0
  $ show-catch-up
  | user                   | review | catch-up | completed |
  |------------------------+--------+----------+-----------|
  | unix-login-for-testing |      2 |          |           |
  | user1                  |      1 |        1 |         1 |
  | user2                  |        |          |         2 |
  |--------------------------------------------------------|

Allow a user to review existing catch-up session for their aliases

  $ fe internal catch-up show-num-lines root -for user1
  1
  $ fe catch-up mark-file root a -for user1
  $ fe internal catch-up show-num-lines root -for user1
  0
  $ show-catch-up
  | user                   | review | completed |
  |------------------------+--------+-----------|
  | unix-login-for-testing |      2 |           |
  | user1                  |      1 |         1 |
  | user2                  |        |         2 |
  |---------------------------------------------|

Do not generate catch-up when reviewing for an alias

  $ fe internal session show-num-lines root  -for user1
  1
  $ fe internal catch-up show-num-lines root -for user1
  0
  $ fe session mark-file root b     -for user1 -reason reason
  $ fe internal session show-num-lines root  -for user1
  0
  $ fe internal catch-up show-num-lines root -for user1
  0
  $ show-catch-up
  | user                   | review | completed |
  |------------------------+--------+-----------|
  | unix-login-for-testing |      2 |           |
  | user1                  |        |         2 |
  | user2                  |        |         2 |
  |---------------------------------------------|
