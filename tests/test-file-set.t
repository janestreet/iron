Start test.

  $ start_test

Setup repo.

  $ setup_repo_without_root
  $ mkdir .fe
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Define_scrutiny normal ((level 50) (description "normal")))
  > (Users user1 user2)
  > EOF
  $ touch .fe/obligations-repo.sexp
  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF
  $ touch .fe.sexp foo bar baz
  $ hg add -q .
  $ hg com -m 'added files'

A bash function for testing a file set.

  $ function file_set {
  >     cat >.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > (Reviewed_by (All_of (Users user1)))
  > (Apply_to $1)
  > EOF
  >     fe ob report | ( sexp query '
  > each
  > (test (index 1) (field review_obligation) (equals (All_of (user1))))
  > (index 0)' || true ) | sort
  > }

Syntax error.

  $ file_set ')'
  ("syntax error"
   ("syntax error when parsing sexp"
    (.fe.sexp:5:10 "unexpected character: ')'")))
  [1]

Invalid sexp.

  $ file_set ''
  ("syntax error"
   ("invalid sexp"
    (.fe.sexp:5:0
     (Failure
      "dot_fe.ml.Declaration.syntax_of_sexp: sum tag \"Apply_to\" has incorrect number of arguments")
     in (Apply_to))))
  [1]

Tests of each syntactic form.

All_files.

  $ file_set 'All_files'
  .fe.sexp
  bar
  baz
  foo

Files.

  $ file_set '(Files)'
  $ file_set '(Files foo)'
  foo
  $ file_set '(Files foo bar)'
  bar
  foo

Glob.

  $ file_set '(Globs)'
  $ file_set '(Globs /)'
  ("syntax error"
   ("invalid sexp" (.fe.sexp:5:17 ("glob may not contain / char." /) in /)))
  [1]
  $ file_set '(Globs *)'
  bar
  baz
  foo
  $ file_set '(Globs a*)'
  ("invalid .fe.sexp file" (.fe.sexp:5:10 ("glob did not match any files" a*)))
  [1]
  $ file_set '(Globs b*)'
  bar
  baz
  $ file_set '(Globs ba*)'
  bar
  baz
  $ file_set '(Globs bar*)'
  bar
  $ file_set '(Globs f*)'
  foo
  $ file_set '(Globs * .*)'
  .fe.sexp
  bar
  baz
  foo

Re2s.

  $ file_set '(Re2s "")'
  ("invalid .fe.sexp file"
   (.fe.sexp:5:10 ("regexp didn't match any files" "")))
  [1]
  $ file_set '(Re2s *)'
  .fe.sexp
  bar
  baz
  foo
  $ file_set '(Re2s .*)'
  .fe.sexp
  bar
  baz
  foo
  $ file_set '(Re2s "[bf].*")'
  bar
  baz
  foo
  $ file_set '(Re2s "(bar|foo)")'
  bar
  foo
  $ file_set '(Re2s "bar" "f.*")'
  bar
  foo

Complement.

  $ file_set '(Complement (Files))'
  .fe.sexp
  bar
  baz
  foo
  $ file_set '(Complement (Complement (Files)))'
  $ file_set '(Complement (Files foo))'
  .fe.sexp
  bar
  baz

Diff.

  $ file_set '(Diff (Files) (Files))'
  $ file_set '(Diff (Files) (Files foo))'
  $ file_set '(Diff (Files foo) (Files))'
  foo
  $ file_set '(Diff (Files foo) (Files bar))'
  foo
  $ file_set '(Diff (Files foo bar) (Files foo))'
  bar

Inter.

  $ file_set '(Inter)'
  .fe.sexp
  bar
  baz
  foo
  $ file_set '(Inter (Files foo))'
  foo
  $ file_set '(Inter (Files foo) (Files foo))'
  foo
  $ file_set '(Inter (Files foo) (Files bar))'
  $ file_set '(Inter (Files foo bar) (Files bar baz))'
  bar
  $ file_set '(Inter (Files foo bar) (Globs *))'
  bar
  foo

Union.

  $ file_set '(Union)'
  $ file_set '(Union (Files foo))'
  foo
  $ file_set '(Union (Files foo) (Files bar))'
  bar
  foo
  $ file_set '(Union (Files foo) (Files bar) (Files baz))'
  bar
  baz
  foo
