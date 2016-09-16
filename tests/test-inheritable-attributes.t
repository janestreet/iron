Start test

  $ start_test

Setup.  Create a feature, create a child feature.

  $ hg init repo
  $ cd repo
  $ remote=$PWD
  $ touch file; hg add file; hg com -m 0
  $ fe create root -remote $remote -d root -permanent
  $ echo a > file; hg commit -m 1
  $ fe create root/child -d child

Show/add/remove inheritable attributes.

  $ fe show root -inheritable-attributes
  ()
  $ fe change root -set-inheritable-whole-feature-followers user1
  $ fe show root -inheritable-attributes |& matches "user1"
  $ fe show child -inheritable-attributes
  ()

Show/add/remove inheritable properties.

  $ fe change root -set-property ticket=FE-290
  $ fe change root -set-inheritable-property ticket=FE-291
  $ fe show root -inheritable-attributes |& matches "ticket"
  $ fe change root -set-inheritable-property not-a-property=yet
  $ fe show root -inheritable-attributes |& matches "not-a-property"

Check basic inheritability works. Add follower to the parent.

  $ fe change root -set-whole-feature-followers user2
  $ fe show root -whole-feature-followers
  (user2)

Check if follower is in the child made previously.

  $ fe show root/child -whole-feature-followers
  ()

Check if follower is in new child.

  $ fe create root/child2 -d child_inherited_follower
  $ fe show child2 -inheritable-attributes |& matches "user1"
  $ fe show child2 -whole-feature-followers
  (user1)
  $ fe show child2 -properties |& matches "FE-291"

Check for conflict case in create. Here, user1 is set as a
whole_feature_reviewer in the command line and also as an inheritable
attribute. Also check that the owners passed into fe create are set as owners
first before inherited owners.


  $ fe change root -set-inheritable-owners user3,unix-login-for-testing,user1,user2 \
  >   -set-inheritable-whole-feature-reviewers user1 \
  >   -set-inheritable-whole-feature-followers ""
  $ fe create root/child3 -d child3 -owners user2 \
  >   -add-whole-feature-reviewers user1,user3
  $ fe show child3 -owners
  (user2 user3 unix-login-for-testing user1)
  $ fe show child3 -whole-feature-reviewers
  (user1 user2 user3)

Check that inheritable attributes recurse down more than one level.

  $ fe change root -set-inheritable-owners user3 \
  > -set-inheritable-whole-feature-followers "" \
  > -set-inheritable-whole-feature-reviewers "" \
  > -set-inheritable-property ticket=FE-290
  $ fe show root -inheritable-attributes
  ((owners (user3)) (properties ((not-a-property yet) (ticket FE-290))))
  $ fe create root/child4 -d child4
  $ fe show child4 -inheritable-attributes
  ((owners (user3)) (properties ((not-a-property yet) (ticket FE-290))))
  $ fe create root/child4/grandchild -d grandchild
  $ fe show grandchild -inheritable-attributes
  ((owners (user3)) (properties ((not-a-property yet) (ticket FE-290))))
  $ fe show grandchild -properties -owners
  ((Owners (unix-login-for-testing user3))
   (Properties ((not-a-property yet) (ticket FE-290))))

Check that show-inheritable-attributes flag works.

  $ fe change root -set-inheritable-crs-shown-in-todo-only-for-users-reviewing false
  $ fe show root -inheritable-attributes
  ((crs_shown_in_todo_only_for_users_reviewing (false)) (owners (user3))
   (properties ((not-a-property yet) (ticket FE-290))))
  $ fe change root -set-inheritable-release-process continuous
  $ fe show root -show-inheritable-attributes \
  >  | grep -B 100 -- '-----' \
  >  | grep -A 100 'inheritable attributes' \
  >  | grep -v -- '-----' \
  >  | single_space
  | inheritable attributes | |
  | CRs shown in todo for | all |
  | owner | user3 |
  | not-a-property | yet |
  | ticket | FE-290 |
  | release process | continuous |
  $ fe change root -remove-inheritable-crs-shown-in-todo-only-for-users-reviewing

  $ fe show child -inheritable-attributes
  ()
  $ fe change child -set-inheritable-release-process continuous
  $ fe show child -inheritable-attributes
  ((release_process (Continuous)))
  $ fe change child -remove-inheritable-release-process
  $ fe show child -inheritable-attributes
  ()
  $ fe change child -add-inheritable-whole-feature-reviewer user1
  $ fe show child -inheritable-attributes
  ((whole_feature_reviewers (user1)))
  $ fe change child -set-inheritable-whole-feature-reviewer user1,user2
  $ fe show child -inheritable-attributes
  ((whole_feature_reviewers (user1 user2)))
  $ fe change child -remove-inheritable-whole-feature-reviewer user1
  $ fe show child -inheritable-attributes
  ((whole_feature_reviewers (user2)))

Check correct behavior if create is called with params:

Set some attributes and properties in the parent.

  $ fe change root -set-inheritable-owners unix-login-for-testing,user2 \
  > -set-inheritable-property prop1=parent-data1 \
  > -set-inheritable-property prop2=parent-data2 \
  > -remove-inheritable-property ticket \
  > -add-inheritable-whole-feature-reviewers unix-login-for-testing

  $ fe change root -remove-inheritable-property not-a-property

Show their value in the parent.

  $ fe show root -inheritable-attributes
  ((owners (unix-login-for-testing user2))
   (properties ((prop1 parent-data1) (prop2 parent-data2)))
   (release_process (Continuous))
   (whole_feature_reviewers (unix-login-for-testing)))

Create a child, and verifies that the attributes and properties are as expected.

  $ fe create root/child_with_params -d child_with_params \
  >   -owners user1 -add-whole-feature-reviewers user2 \
  >   -property prop1=child-data1

  $ fe show child_with_params -owners
  (user1 unix-login-for-testing user2)

  $ fe show child_with_params -whole-feature-reviewers
  (unix-login-for-testing user1 user2)

  $ fe show child_with_params -inheritable-attributes
  ((owners (unix-login-for-testing user2))
   (properties ((prop1 parent-data1) (prop2 parent-data2)))
   (release_process (Continuous))
   (whole_feature_reviewers (unix-login-for-testing)))

  $ fe show child_with_params -properties
  ((prop1 child-data1) (prop2 parent-data2))

  $ fe show child_with_params -release-process
  Continuous
