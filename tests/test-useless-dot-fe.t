  $ start_test
  $ setup_repo_without_root
  $ mkdir .fe
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Define_scrutiny normal ((level 50) (description "normal")))
  > (Users user1)
  > Disallow_useless_dot_fe
  > EOF
  $ cat >.fe/obligations-repo.sexp <<EOF
  > EOF
  $ cat >.fe/.fe.sexp <<EOF
  > (Scrutiny normal)
  > (Owner user1)
  > (Apply_to All_files)
  > EOF
  $ hg add -q
  $ fe ob check

Add a useless .fe.sexp.

  $ cp .fe/.fe.sexp .fe.sexp
  $ hg add .fe.sexp
  $ fe ob check
  ("useless .fe.sexp file" .fe.sexp)
  [1]

Make it useful by adding a file.

  $ touch file
  $ hg add file
  $ fe ob check

Make it useless again.

  $ hg forget file
  $ fe ob check
  ("useless .fe.sexp file" .fe.sexp)
  [1]

Make it useful by using it in a subdirectory.

  $ ( cat .fe/.fe.sexp; echo 'Used_in_subdirectory' ) >.fe.sexp
  $ mkdir subdir
  $ touch subdir/file
  $ hg add subdir/file
  $ fe ob check
