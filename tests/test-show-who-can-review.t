Start test.

  $ start_test

Setup a repo with two files:

  $ setup_sample_repo_and_root two-users
  $ feature_to_server root
  $ seq 1 3 > a
  $ seq 1 3 > b
  $ hg addremove . -q
  $ hg commit -m "add some lines"
  $ feature_to_server root
  $ fe show
  root
  ====
  root
  
  |-------------------------------------------------|
  | attribute              | value                  |
  |------------------------+------------------------|
  | next step              | enable-review          |
  | owner                  | unix-login-for-testing |
  | whole-feature reviewer | unix-login-for-testing |
  | seconder               | not seconded           |
  | review is enabled      | false                  |
  | CRs are enabled        | true                   |
  | reviewing              | unix-login-for-testing |
  | is permanent           | true                   |
  | tip                    | 225d4bac9981           |
  | base                   | 0f5b3abd13b6           |
  |-------------------------------------------------|
  
  |---------------------------------|
  | user                   | review |
  |------------------------+--------|
  | unix-login-for-testing |      6 |
  | user1                  |      3 |
  | user2                  |      3 |
  |---------------------------------|

When reviewing not enabled should have blank output.

  $ fe show -who-can-review
  $ fe enable-review
  $ fe show -who-can-review
  unix-login-for-testing

Now add in whole feature reviewers.

  $ fe change -add-whole-feature-reviewers jdoe3
  $ fe show -who-can-review
  unix-login-for-testing

  $ fe widen-reviewing
  $ fe show -who-can-review
  jdoe3
  unix-login-for-testing

After we review.

  $ fe tools mark-fully-reviewed root
  $ fe show -who-can-review
  jdoe3

Enable more users to review.

  $ fe change root -set-reviewing-all
  $ fe show -reviewing
  all
  $ fe show -who-can-review
  jdoe3
  user1
  user2

Reviewing restricted to Only user1 user2.

  $ fe change root -set-reviewing 'user1,user2'
  $ fe show -reviewing
  (user1 user2)
  $ fe show -who-can-review
  user1
  user2
