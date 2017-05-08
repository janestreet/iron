Start test.

  $ start_test

Create hg repo.

  $ setup_repo_and_root file
  $ echo change >file; hg com -m 'change'
  $ feature_to_server root -fake-valid

Tag the root tip.

  $ hg up -q -r 0  # so that tagging doesn't change [root]
  $ hg tag -f -r root 'root-111.11+24'
  $ hg tag -r root 'root-111.12'
  $ hg log -r root '--template={tags}\n'
  root-111.11+24 root-111.12

Create a child, and it has a nice name for its base.

  $ fe create root/child -desc child
  $ feature_to_server root/child -fake-valid
  $ fe show
  root/child
  ==========
  child
  
  |-----------------------------------------------------|
  | attribute              | value                      |
  |------------------------+----------------------------|
  | next step              | add code                   |
  | owner                  | unix-login-for-testing     |
  | whole-feature reviewer | unix-login-for-testing     |
  | seconder               | not seconded               |
  | review is enabled      | false                      |
  | CRs are enabled        | true                       |
  | reviewing              | unix-login-for-testing     |
  | is permanent           | false                      |
  | tip                    | root-111.12 [289e0fc393fa] |
  | base                   | root-111.12 [289e0fc393fa] |
  |-----------------------------------------------------|

The base gets a nice name even if the rev is not present when [fe create] starts.

  $ cd ..
  $ hg clone -q -r 1ef1f9600b48 repo repo2
  $ cd repo2
  $ hg log -r 289e0fc393fa |& matches "unknown revision"
  [255]
  $ IRON_OPTIONS='((workspaces false))' fe create root/child2 -desc child2
  $ feature_to_server root/child2 -fake-valid
  $ fe show
  root/child2
  ===========
  child2
  
  |-----------------------------------------------------|
  | attribute              | value                      |
  |------------------------+----------------------------|
  | next step              | add code                   |
  | owner                  | unix-login-for-testing     |
  | whole-feature reviewer | unix-login-for-testing     |
  | seconder               | not seconded               |
  | review is enabled      | false                      |
  | CRs are enabled        | true                       |
  | reviewing              | unix-login-for-testing     |
  | is permanent           | false                      |
  | tip                    | root-111.12 [289e0fc393fa] |
  | base                   | root-111.12 [289e0fc393fa] |
  |-----------------------------------------------------|
