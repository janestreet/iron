  $ start_test

-- Test completion on feature names, which requires making RPCs to the server.

  $ setup_repo_and_root file

  $ completion-test fe show ''
  root
  $ completion-test fe show r
  root
  $ completion-test fe show root
  root
  $ completion-test fe show a
  $ fe create root/foo1 -description ''
  $ fe create root/foo2 -description ''
  $ fe create root/foo2/bar -description ''
  $ fe create root/foo2/quux -description ''
  $ fe create root/quux -description ''
  $ fe create root/quux/rrr -description ''
  $ fe create root/quux/foo -description ''
  $ fe create root/quux/bar -description ''
  $ fe create root/quux/baz -description ''
  $ fe create root/duux -description ''
  $ fe create root/duux/baz -description ''
  $ completion-test fe show ro
  root/
  root/x

The /x alternative is added to keep bash from from adding a space after the
singleton-set completion of "root".

  $ completion-test fe show root
  root/
  root/x

Note that foo2 has a slash (as it has a child) and foo1 doesn't (no children):

  $ completion-test fe show root/f
  root/foo1
  root/foo2/

  $ completion-test fe show root/foo
  root/foo1
  root/foo2/

Test completion by unanchored partial path.

  $ completion-test fe show rr
  root/quux/rrr

  $ completion-test fe show f
  foo
  foo1
  foo2/

  $ completion-test fe show foo1
  root/foo1

  $ completion-test fe show foo2
  root/foo2/
  root/foo2/x

  $ completion-test fe show bar
  bar

  $ completion-test fe show d
  root/duux/
  root/duux/x

  $ completion-test fe show quux/
  quux/
  quux/bar
  quux/baz
  quux/foo
  quux/rrr

[fe create] only does full-path completion.

  $ completion-test fe create rr
  $ completion-test fe create quux
  $ completion-test fe create root/quux
  root/quux/
  root/quux/x

-- Test: Completion on [fe unarchive] only shows archived features:

  $ fe archive root/foo1
  $ fe list -archived -depth 2
  |------------------------------------------------------------------------------|
  | feature | feature id                           | archived at                 |
  |---------+--------------------------------------+-----------------------------|
  | root    |                                      |                             |
  |   foo1  | * | * | (glob)
  |------------------------------------------------------------------------------|

  $ completion-test fe unarchive root/f
  root/foo1

We complete all the way, above, because only root/foo1 is archived, not root/foo2.

-- Test completion on attributes.

  $ completion-test fe change feature -
  -add-inheritable-owners
  -add-inheritable-send-email-to
  -add-inheritable-send-email-upon
  -add-inheritable-whole-feature-followers
  -add-inheritable-whole-feature-reviewers
  -add-owners
  -add-reviewing
  -add-send-email-to
  -add-send-email-upon
  -add-whole-feature-followers
  -add-whole-feature-reviewers
  -help
  -lock
  -remove-inheritable-crs-shown-in-todo-only-for-users-reviewing
  -remove-inheritable-owners
  -remove-inheritable-property
  -remove-inheritable-release-process
  -remove-inheritable-send-email-to
  -remove-inheritable-send-email-upon
  -remove-inheritable-who-can-release-into-me
  -remove-inheritable-whole-feature-followers
  -remove-inheritable-whole-feature-reviewers
  -remove-inheritable-xcrs-shown-in-todo-only-for-users-reviewing
  -remove-owners
  -remove-property
  -remove-reviewing
  -remove-send-email-to
  -remove-send-email-upon
  -remove-whole-feature-followers
  -remove-whole-feature-reviewers
  -set-base
  -set-crs-are-enabled
  -set-crs-shown-in-todo-only-for-users-reviewing
  -set-description
  -set-inheritable-crs-shown-in-todo-only-for-users-reviewing
  -set-inheritable-owners
  -set-inheritable-property
  -set-inheritable-release-process
  -set-inheritable-send-email-to
  -set-inheritable-send-email-upon
  -set-inheritable-who-can-release-into-me
  -set-inheritable-whole-feature-followers
  -set-inheritable-whole-feature-reviewers
  -set-inheritable-xcrs-shown-in-todo-only-for-users-reviewing
  -set-is-permanent
  -set-lines-required-to-separate-ddiff-hunks
  -set-owners
  -set-property
  -set-release-process
  -set-reviewing
  -set-reviewing-all
  -set-reviewing-none
  -set-reviewing-whole-feature-only
  -set-send-email-to
  -set-send-email-upon
  -set-who-can-release-into-me
  -set-whole-feature-followers
  -set-whole-feature-reviewers
  -set-xcrs-shown-in-todo-only-for-users-reviewing
  -verbose

  $ completion-test fe change feature -ad
  -add-inheritable-owners
  -add-inheritable-send-email-to
  -add-inheritable-send-email-upon
  -add-inheritable-whole-feature-followers
  -add-inheritable-whole-feature-reviewers
  -add-owners
  -add-reviewing
  -add-send-email-to
  -add-send-email-upon
  -add-whole-feature-followers
  -add-whole-feature-reviewers

-- Test: usernames

  $ completion-test fe change feature -add-owners u
  unix-login-for-testing
  user
  user1
  user2
  user3

  $ completion-test fe tools mark-fully-reviewed feature -for al
  all

  $ completion-test fe tools mark-fully-reviewed feature -for-all-but user2,un
  user2,unix-login-for-testing
  user2,unix-login-for-testing,

Trailing comma if there is a single completion string:

  $ completion-test fe change feature -add-owners user2,un
  user2,unix-login-for-testing
  user2,unix-login-for-testing,

Same if the name is complete already.

  $ completion-test fe change feature -add-owners user2,user3
  user2,user3
  user2,user3,

Don't repeat completions:

  $ completion-test fe change feature -add-owners user2,us
  user2,user
  user2,user1
  user2,user3

  $ completion-test fe change feature -add-owners user2,
  user2,a
  user2,b
  user2,c
  user2,file-follower
  user2,file-owner
  user2,jdoe1
  user2,jdoe2
  user2,jdoe3
  user2,jdoe4
  user2,new-owner
  user2,owner
  user2,seconder
  user2,this-user-has-only-this-cr-soon
  user2,unix-login-for-testing
  user2,user
  user2,user1
  user2,user3

Test the completion tool exposed for scripts.

  $ function tools_complete {
  >     fe tools complete-feature-path $@ <<EOF
  > jane/genpos/2.2.0
  > jane/grass/2.2
  > jane/grass/2.2/summary-rpc-watcher
  > EOF
  > }

  $ tools_complete '2.2'
  2.2.0
  2.2/

  $ tools_complete 'g'
  genpos/
  grass/

  $ tools_complete 'gen'
  jane/genpos/
  jane/genpos/x

  $ tools_complete 'g' -full-name

  $ tools_complete 'j' -full-name
  jane/
  jane/x

  $ tools_complete 'jane/g' -full-name
  jane/genpos/
  jane/grass/
