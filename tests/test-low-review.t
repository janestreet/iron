  $ start_test
  $ setup_repo_without_root
  $ mkdir .fe
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Define_scrutiny critical_path
  >   ((level 85)
  >    (description "critical path")
  >    (min_file_reviewers 1)))
  > (Define_scrutiny normal ((level 50) (description "normal")))
  > (Users unix-login-for-testing)
  > EOF
  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Define_build_projection base
  >   ((default_scrutiny critical_path)
  >    (require_low_review_file true)))
  > EOF
  $ cat >.fe/.fe.sexp <<EOF
  > (Build_projections base)
  > (Owner unix-login-for-testing)
  > (Scrutiny critical_path)
  > (Reviewed_by (All_of (Users unix-login-for-testing)))
  > (Apply_to All_files)
  > EOF
  $ hg add -q .

There must be a low-review file even if it is empty.

  $ fe ob check
  ("missing low-review file" .fe/low-review-in-base)
  [1]
  $ touch .fe/low-review-in-base
  $ fe ob check
  ("untracked low-review file" .fe/low-review-in-base)
  [1]
  $ hg add -q .
  $ fe ob check

Extraneous low-review files.

  $ echo '.fe.sexp' >.fe/low-review-in-base
  $ fe ob check
  (.fe/low-review-in-base:0:0 ("files are not low review" (.fe.sexp)))
  [1]

If a file has low review, it must be included in the low-review file.

  $ cat >.fe/.fe.sexp <<EOF
  > (Build_projections base)
  > (Owner unix-login-for-testing)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF
  $ fe ob check
  (.fe/low-review-in-base:0:0
   ("missing low-review files"
    (.fe/.fe.sexp .fe/low-review-in-base .fe/obligations-global.sexp
     .fe/obligations-repo.sexp)))
  [1]

State the low-review, and then we're OK.

  $ fe ob update-low-review-files
  $ fe ob check

.fe.sexp files inferred from ancestors aren't low review.

  $ ( cat .fe/.fe.sexp; echo Used_in_subdirectory ) >.fe.sexp
  $ hg add .fe.sexp
  $ hg forget .fe/.fe.sexp
  $ rm .fe/.fe.sexp
  $ hg st
  A .fe.sexp
  A .fe/low-review-in-base
  A .fe/obligations-global.sexp
  A .fe/obligations-repo.sexp
  $ fe ob update-low-review-files
  $ grep -q -F .fe/.fe.sexp .fe/low-review-in-base
  [1]
  $ fe ob check
