Start test.

  $ start_test

Check that one can add an admin from the server box, and then add other admins as an admin.

  $ fe admin users admins get

  $ IRON_USER=user1 fe admin users admins add user1
  (error
   (user-set-change
    ("unauthorized RPC by user -- admin privileges required"
     ((user user1) (users_with_admin_privileges (unix-login-for-testing))))))
  [1]

  $ fe admin users admins add user1

  $ fe admin users admins get
  user1

  $ IRON_USER=user1 fe admin users admins add user2

  $ fe admin users admins get
  user1
  user2

Check the semantic of [-idempotent].  When the command fails, this has no effect
on the server.

  $ IRON_USER=user1 fe admin users admin add user2 user3 user4
  (error (user-set-change ("user already in the set [admins]" (user2))))
  [1]

  $ fe admin users admins get
  user1
  user2

  $ IRON_USER=user1 fe admin users admin add user2 user3 user4 -idempotent

  $ fe admin users admins get
  user1
  user2
  user3
  user4

Test the remove command a little bit too.

  $ IRON_USER=user1 fe admin users admin remove jdoe jsmith user2 user3 user4
  (error (user-set-change ("users not in the set [admins]" (jdoe jsmith))))
  [1]

  $ fe admin users admins get
  user1
  user2
  user3
  user4

  $ IRON_USER=user1 fe admin users admin remove jdoe jsmith user2 user3 user4 -idempotent

  $ fe admin users admins get
  user1
