Start test.

  $ start_test

Create .fe files.

  $ setup_repo_without_root
  $ mkdir .fe
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Users user1)
  > (Define_scrutiny normal
  >   ((level 0)
  >    (description "normal scrutiny description")
  >    (min_file_reviewers 0)
  >    (max_file_reviewers 10)
  >   ))
  > EOF
  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Define_build_projection p1 ((default_scrutiny normal)))
  > (Define_build_projection p2 ((default_scrutiny normal)))
  > (Define_build_projection p3 ((default_scrutiny normal)))
  > EOF
  $ cat >.fe/.fe.sexp <<EOF
  > (Build_projections p1 p2)
  > (Owner user1)
  > (Reviewed_by (All_of (Users user1)))
  > (Apply_to All_files)
  > EOF
  $ hg add -q
  $ hg com -m 'added .fe files'

List projections.

  $ fe ob list-projections
  p1
  p2
  p3

Unused projection.

  $ fe ob check
  ("unused build projections -- please remove from obligations-repo.sexp" (p3))
  [1]

Remove the unused projection.

  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Define_build_projection p1 ((default_scrutiny normal)))
  > (Define_build_projection p2 ((default_scrutiny normal)))
  > EOF
  $ fe ob check

Ask for no projections, get no files.

  $ fe ob projection

Asking for non-existent projection is an error.

  $ fe ob projection zzz
  ("undefined projections" (zzz))
  [1]

Asking for a single projection.

  $ fe ob projection p1
  .fe/.fe.sexp
  .fe/obligations-global.sexp
  .fe/obligations-repo.sexp
  $ fe ob projection p2
  .fe/.fe.sexp
  .fe/obligations-global.sexp
  .fe/obligations-repo.sexp

A file in multiple projections.

  $ cat >.fe/.fe.sexp <<EOF
  > (Build_projections p1)
  > (Owner user1)
  > (Reviewed_by (All_of (Users user1)))
  > (Apply_to All_files)
  > (Build_projections p2)
  > (Apply_to (Files .fe.sexp))
  > EOF
  $ fe ob projection p1
  .fe/.fe.sexp
  .fe/obligations-global.sexp
  .fe/obligations-repo.sexp
  $ fe ob projection p2
  .fe/.fe.sexp
  $ fe ob projection p1 p2
  .fe/.fe.sexp
  .fe/obligations-global.sexp
  .fe/obligations-repo.sexp
