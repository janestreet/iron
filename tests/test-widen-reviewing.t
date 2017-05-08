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

Begin with empty reviewing set.

  $ fe change -set-reviewing-none

When reviewing not enabled, raise error.

  $ fe widen-review
  Review is not enabled; must first run [fe enable-review].
  [1]
  $ fe enable
  $ fe widen-review
  $ fe show -reviewing
  "whole-feature reviewers"

Now add in whole feature reviewers.

  $ fe change -add-whole-feature-reviewers jdoe3
  $ fe change -remove-reviewing jdoe3
  $ fe show -reviewing
  (unix-login-for-testing)
  $ fe widen-review
  $ fe show -reviewing
  "whole-feature reviewers"

Widening review again should raise an error since not seconded.

  $ fe widen-review
  Feature is not seconded; a seconder must run [fe second].
  [1]

Adds the file owner if needed.

  $ fe change -add-whole-feature-reviewers user1
  $ fe change -set-reviewing user1
  $ fe show -reviewing
  (user1)
  $ fe widen-review
  $ fe show -reviewing
  (unix-login-for-testing user1)

Then it expands to w-f review set.

  $ fe widen-review
  $ fe show -reviewing
  "whole-feature reviewers"

Unless it is obligated to include non w-f reviewers.

  $ fe change -set-reviewing user2
  $ fe widen-review
  $ fe show -reviewing
  (unix-login-for-testing user2)
  $ fe widen-review
  $ fe show -reviewing
  (jdoe3 unix-login-for-testing user1 user2)

Cannot add non-w-f reviewers if unseconded.

  $ fe widen-review
  Feature is not seconded; a seconder must run [fe second].
  [1]
  $ IRON_USER=user1 fe second

Prefers to add only w-f reviewers if it can.

  $ fe change -remove-reviewing user1,user2
  $ fe widen-review
  $ fe show -reviewing
  ("all but" (user2))
  $ fe widen-review
  $ fe show -reviewing
  all

Fails if everyone is reviewing.

  $ fe widen-review
  Cannot widen review; everyone is already reviewing.
  [1]
