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
  $ hg add -q .
  $ hg com -m 'added files'
  $ fe ob check

Can't say [(Fewer_than_min_reviewers true)] when it's false.

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > (Local (Fewer_than_min_reviewers true) (Apply_to (Files .fe.sexp)))
  > EOF
  $ fe ob check
  ("invalid .fe.sexp file"
   (.fe/.fe.sexp:0:0
    ("invalid file attributes"
     (((file .fe.sexp)
       (error
        ("there are 0 reviewers and only 0 required, so must have (Fewer_than_min_reviewers false)"
         (All_of ()))))))))
  [1]

But, if there are fewer-than-min-reviewers, you must say it.

  $ cat >.fe/obligations-global.sexp <<EOF
  > (Users user1 user2 user3)
  > (Define_scrutiny normal ((level 50) (description "normal") (min_file_reviewers 1)))
  > EOF
  $ fe ob check
  ("invalid .fe.sexp file"
   (.fe/.fe.sexp:0:0
    ("invalid file attributes"
     (((file obligations-global.sexp)
       (error ("need 1 reviewer but there are only 0" (All_of ()))))
      ((file obligations-repo.sexp)
       (error ("need 1 reviewer but there are only 0" (All_of ()))))))))
  [1]

Fix the problem by asserting Fewer_than_min_reviewers.

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Local (Fewer_than_min_reviewers true) (Apply_to All_files))
  > EOF
  $ fe ob check

Or, add the reviewers.

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Reviewed_by (All_of (Users user1)))
  > (Apply_to All_files)
  > EOF
  $ fe ob check

Can't say [(Fewer_than_min_reviewers false)] and [(Fewer_than_min_reviewers true)].

  $ cat >.fe/.fe.sexp <<EOF
  > (Fewer_than_min_reviewers false)
  > (Apply_to All_files)
  > (Fewer_than_min_reviewers true)
  > (Apply_to All_files)
  > EOF
  $ fe ob check
  ("invalid .fe.sexp file"
   (.fe/.fe.sexp:4:0
    ("inconsistent Fewer_than_min_reviewers for .fe.sexp" (false true))))
  [1]

More_than_max_reviewers.

  $ cat >.fe/obligations-global.sexp <<EOF
  > (Users user1 user2 user3)
  > (Define_scrutiny normal ((level 50) (description "normal") (max_file_reviewers 1)))
  > EOF

Can't say [(More_than_max_reviewers true)] when it's false.

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > (Local (More_than_max_reviewers true) (Apply_to (Files .fe.sexp)))
  > EOF
  $ fe ob check
  ("invalid .fe.sexp file"
   (.fe/.fe.sexp:0:0
    ("invalid file attributes"
     (((file .fe.sexp)
       (error
        ("there are 0 reviewers, with 1 allowed, so must have (More_than_max_reviewers false)"
         (All_of ()))))))))
  [1]

But, if there are more-than-max-reviewers, you must say it.

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Reviewed_by (All_of (Users user1 user2)))
  > (Apply_to All_files)
  > EOF
  $ fe ob check
  ("invalid .fe.sexp file"
   (.fe/.fe.sexp:0:0
    ("invalid file attributes"
     (((file .fe.sexp)
       (error
        ("allowed up to 1 reviewer, but there are 2" (All_of (user1 user2)))))
      ((file obligations-global.sexp)
       (error
        ("allowed up to 1 reviewer, but there are 2" (All_of (user1 user2)))))
      ((file obligations-repo.sexp)
       (error
        ("allowed up to 1 reviewer, but there are 2" (All_of (user1 user2)))))))))
  [1]

Fix the problem by asserting more_than_max_reviewers.

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Reviewed_by (All_of (Users user1 user2)))
  > (Local (More_than_max_reviewers true) (Apply_to All_files))
  > EOF
  $ fe ob check

Or, remove a reviewer.

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Reviewed_by (All_of (Users user1)))
  > (Apply_to All_files)
  > EOF
  $ fe ob check

Can't say [(More_than_max_reviewers false)] and [(More_than_max_reviewers true)].

  $ cat >.fe/.fe.sexp <<EOF
  > (More_than_max_reviewers false)
  > (Apply_to All_files)
  > (More_than_max_reviewers true)
  > (Apply_to All_files)
  > EOF
  $ fe ob check
  ("invalid .fe.sexp file"
   (.fe/.fe.sexp:4:0
    ("inconsistent More_than_max_reviewers for .fe.sexp" (false true))))
  [1]
