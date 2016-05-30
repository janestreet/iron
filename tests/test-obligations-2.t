Start test.

  $ start_test

Create .fe files.

  $ setup_repo_and_root .fe/.fe.sexp
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Users user1 user2 user3 foo1 foo2 foo3)
  > (Define_scrutiny normal
  >   ((level 0)
  >    (description "normal scrutiny description")
  >    (min_file_reviewers 0)
  >    (max_file_reviewers 10)
  >   ))
  > (Define_group foos   (foo1  foo2  foo3))
  > (Define_group users (user1 user2 user3))
  > EOF
  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Users bar1 bar2)
  > (Define_group bars (bar1 bar2))
  > EOF
  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Reviewed_by (All_of (Users user1)))
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF
  $ cat >.fe.sexp <<EOF
  > (Owner user1)
  > (Reviewed_by (All_of (Users user1)))
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF
  $ hg add -q
  $ hg com -m 'added files'
  $ REV1=$(hg log -r . --template='{node}\n')
  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Users bar1 bar2 bar3)
  > (Define_group bars (bar1 bar2 bar3))
  > EOF
  $ hg com -m 'added user bar3'
  $ feature_to_server root

One can get the list of known users.

  $ fe obligations list-users
  bar1
  bar2
  bar3
  foo1
  foo2
  foo3
  user1
  user2
  user3

One can get the list of known groups.

  $ fe obligations list-groups
  bars
  foos
  users

One can get the list of users in a given set of groups.

  $ fe obligations list-users-in-groups '(or bars foos)'
  bar1
  bar2
  bar3
  foo1
  foo2
  foo3

  $ fe obligations list-users-in-groups abelian
  ("undefined group" (abelian))
  [1]

One can also do so at a specific revision.

  $ fe obligations list-users -rev $REV1
  bar1
  bar2
  foo1
  foo2
  foo3
  user1
  user2
  user3

  $ fe obligations list-users-in-groups bars -rev $REV1
  bar1
  bar2

Check that completion works on groups blang.

  $ completion-test fe obligations list-users-in-groups ba
  bars

  $ completion-test fe obligations list-users-in-groups '(and us'
  (and users

It is an error to a group more than once.

  $ (hg cat .fe/obligations-repo.sexp -r $(fe show -tip) ; cat ) >.fe/obligations-repo.sexp <<EOF
  > (Define_group foos (foo1  foo2  foo3))
  > EOF

  $ fe obligations check |& \
  >   matches "remove groups from .fe/obligations-repo.sexp that are also in .fe/obligations-global.sexp.*foos"
  [1]

  $ (hg cat .fe/obligations-repo.sexp -r $(fe show -tip) ; cat ) >.fe/obligations-repo.sexp <<EOF
  > (Define_group one (user1 foo1))
  > (Define_group one (user1 foo1))
  > EOF

  $ fe obligations check |& \
  >   matches "multiply defined group.*one"
  [1]

It is an error to define a group with unknown users.

  $ (hg cat .fe/obligations-repo.sexp -r $(fe show -tip) ; cat ) >.fe/obligations-repo.sexp <<EOF
  > (Define_group baz (baz1 baz2 baz3))
  > EOF

  $ fe obligations check |& \
  >   matches "groups with unknown users.*((group baz) (unknown_users (baz1 baz2 baz3)))"
  [1]

  $ hg revert -a -q

It is an error to define a group with duplicated users in it.

  $ (hg cat .fe/obligations-repo.sexp -r $(fe show -tip) ; cat ) >.fe/obligations-repo.sexp <<EOF
  > (Define_group one (user1 foo1 user1))
  > EOF

  $ fe obligations check |& \
  >   matches ".fe/obligations-repo.sexp.*duplicate element in set.*user1"
  [1]

  $ hg revert -a -q

It is an error to refer in a .fe.sexp file to an non existing group.

  $ cat >.fe.sexp <<EOF
  > (Owner user1)
  > (Reviewed_by (All_of (Group abelian)))
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF

  $ fe obligations check
  ("invalid .fe.sexp file" (.fe.sexp:2:13 ("no such group" abelian)))
  [1]

  $ hg revert -a -q
