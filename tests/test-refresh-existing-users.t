Start test.

  $ start_test

Restrict the set of employees.

  $ fe admin users update-valid-users-and-aliases -stdin <<EOF
  > ((username jsmith))
  > ((username jdoe4))
  > ((username unix-login-for-testing))
  > EOF
  $ fe admin users refresh-existing-users

Now [jsmith] and [jdoe4] should be existing user names.

  $ fe internal dump user-info existing-users
  (jdoe4 jsmith unix-login-for-testing)

Setup a repository.

  $ setup_repo_and_root file1.ml
  $ cat > file1.ml <<EOF
  > (* $CR jdoe4 for invalid-user: cr3 *)
  > EOF
  $ hg commit -m "add crs"
  $ feature_to_server root -fake-valid-obligations

Now [invalid-user] and [unix-login-for-testing] should be existing
user names.  [invalid-user] enters the set because he has a cr and
[unix-login-for-testing] enters the set as an owner.

  $ fe admin users refresh-existing-users
  $ fe internal dump user-info existing-users
  (invalid-user jdoe4 jsmith unix-login-for-testing)
  $ completion-test fe change feature -add-owners inv
  invalid-user
  invalid-user,

Create a bookmark without feature.

  $ fe internal rpc-to-server call synchronize-state <<EOF
  > ((remote_repo_path $PWD)
  >  (bookmarks
  >   (((bookmark book)
  >     (rev_info
  >       ((first_12_of_rev deadbeef1234)
  >        (rev_author_or_error (Ok bookmark-user))))
  >     (status Pending_or_working_on_it)
  >     (continuous_release_status Not_working_on_it)
  >     (compilation_status ())))))
  > EOF
  ((bookmarks_to_rerun ()))

Now [bookmark-user] should appear.

  $ fe admin users refresh-existing-users
  $ fe internal dump user-info existing-users
  (bookmark-user invalid-user jdoe4 jsmith unix-login-for-testing)

When the cr is resolved, [invalid-user] should disappear.

  $ cat > file1.ml <<EOF
  > (* $XCR jdoe4 for invalid-user: cr3 *)
  > EOF
  $ hg commit -m "cr resolved"
  $ feature_to_server root -fake-valid-obligations
  $ fe admin users refresh-existing-users
  $ fe internal dump user-info existing-users
  (bookmark-user jdoe4 jsmith unix-login-for-testing)
  $ completion-test fe change feature -add-owners inv

If we remove [jsmith] as an employee, he should stop appearing.

  $ fe admin users update-valid-users-and-aliases -stdin <<EOF
  > ((username jdoe4))
  > EOF
  $ fe admin users refresh-existing-users
  $ fe internal dump user-info existing-users
  (bookmark-user jdoe4 unix-login-for-testing)
