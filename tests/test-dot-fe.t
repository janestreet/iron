Start test.

  $ start_test

Setup repo.

  $ setup_repo_without_root
  $ mkdir .fe

Missing obligations-repo.sexp.

  $ fe ob check |& matches "Error loading .fe/obligations-repo.sexp"
  [1]
  $ cat >.fe/obligations-repo.sexp <<EOF
  > EOF
  $ hg add -q .
  $ hg com -m 'added obligations-repo.sexp'

Missing obligations-global.sexp.

  $ fe ob check
  ("Error finding .fe/obligations-global.sexp: looked for these files in turn, but none exist"
   (.fe/obligations-global.sexp ../.fe/obligations-global.sexp scaffold.sexp))
  [1]
  $ cat >.fe/obligations-global.sexp <<EOF
  > EOF
  $ hg add -q
  $ hg com -m 'added obligations-global.sexp'

Missing .fe.sexp.

  $ fe ob check
  ("no .fe.sexp in or above directory" .fe)
  [1]
  $ cat >.fe/.fe.sexp <<EOF
  > EOF
  $ hg add -q .
  $ hg com -m 'added .fe.sexp'

Missing owners.

  $ fe ob check
  ("invalid .fe.sexp file"
   (.fe/.fe.sexp:0:0
    ("invalid file attributes"
     (((file .fe.sexp) (error "missing owner"))
      ((file obligations-global.sexp) (error "missing owner"))
      ((file obligations-repo.sexp) (error "missing owner"))))))
  [1]

Define the owner, but there are no users defined yet.

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Apply_to (Files .fe.sexp))
  > EOF
  $ fe ob check
  ("invalid .fe.sexp file"
   (.fe/.fe.sexp:1:0
    ("users not in obligations-global.sexp or obligations-repo.sexp" (user1))))
  [1]

Define users.

  $ cat >.fe/obligations-global.sexp <<EOF
  > (Users user1)
  > EOF

Now the owner is fixed for .fe.sexp, but there are still problems.

  $ fe ob check
  ("invalid .fe.sexp file"
   (.fe/.fe.sexp:0:0
    ("invalid file attributes"
     (((file .fe.sexp) (error "missing scrutiny"))
      ((file obligations-global.sexp) (error "missing owner"))
      ((file obligations-repo.sexp) (error "missing owner"))))))
  [1]

Fix all owners, but still missing scrutiny.

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Apply_to All_files)
  > EOF
  $ fe ob check
  ("invalid .fe.sexp file"
   (.fe/.fe.sexp:0:0
    ("invalid file attributes"
     (((file .fe.sexp) (error "missing scrutiny"))
      ((file obligations-global.sexp) (error "missing scrutiny"))
      ((file obligations-repo.sexp) (error "missing scrutiny"))))))
  [1]

Undefined scrutiny.

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny high)
  > (Apply_to All_files)
  > EOF
  $ fe ob check
  ("invalid .fe.sexp file" (.fe/.fe.sexp:2:0 ("undefined scrutiny" high)))
  [1]

Define scrutiny.

  $ cat >.fe/obligations-global.sexp <<EOF
  > (Users user1)
  > (Define_scrutiny high
  >   ((level 85)
  >    (description "high scrutiny description")
  >   ))
  > EOF

# Obligations are now good.

  $ fe ob check
  $ fe ob show .fe/.fe.sexp
  ((build_projections ()) (fewer_than_min_reviewers false) (followers ())
   (is_read_by_whole_feature_reviewers true) (more_than_max_reviewers false)
   (owner user1) (review_obligation (All_of ())) (scrutiny_level 85)
   (scrutiny_name high))

Duplicate user in obligations-repo.sexp.

  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Users user1)
  > EOF
  $ fe ob check
  ($TESTTMP/repo/.fe/obligations-repo.sexp:0:0
   ("remove users from .fe/obligations-repo.sexp that are also in .fe/obligations-global.sexp (which comes from the scaffolded repo)"
    (user1)))
  [1]
  $ cat >.fe/obligations-repo.sexp <<EOF
  > EOF

Undefined build projection.

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny high)
  > (Build_projections base)
  > (Apply_to All_files)
  > EOF
  $ fe ob show .fe/.fe.sexp
  ("invalid .fe.sexp file"
   (.fe/.fe.sexp:3:0 ("undefined build projection" (base))))
  [1]

Define build projection.

  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Define_build_projection base ((default_scrutiny high)))
  > EOF
  $ fe ob show .fe/.fe.sexp
  ((build_projections (base)) (fewer_than_min_reviewers false) (followers ())
   (is_read_by_whole_feature_reviewers true) (more_than_max_reviewers false)
   (owner user1) (review_obligation (All_of ())) (scrutiny_level 85)
   (scrutiny_name high))

Pick up scrutiny from build projection only.

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Build_projections base)
  > (Apply_to All_files)
  > EOF
  $ fe ob show .fe/.fe.sexp
  ((build_projections (base)) (fewer_than_min_reviewers false) (followers ())
   (is_read_by_whole_feature_reviewers true) (more_than_max_reviewers false)
   (owner user1) (review_obligation (All_of ())) (scrutiny_level 85)
   (scrutiny_name high))

Explicit scrutiny overrides build-projection default scrutiny.

  $ cat >.fe/obligations-global.sexp <<EOF
  > (Users user1 user2 user3)
  > (Define_scrutiny high
  >   ((level 85)
  >    (description "high scrutiny description")
  >   ))
  > (Define_scrutiny normal
  >   ((level 50)
  >    (description "normal scrutiny description")
  >   ))
  > EOF
  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Build_projections base)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF
  $ fe ob show .fe/.fe.sexp
  ((build_projections (base)) (fewer_than_min_reviewers false) (followers ())
   (is_read_by_whole_feature_reviewers true) (more_than_max_reviewers false)
   (owner user1) (review_obligation (All_of ())) (scrutiny_level 50)
   (scrutiny_name normal))

Inconsistent scrutinies.

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Build_projections base)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > (Scrutiny high)
  > (Apply_to (Files .fe.sexp))
  > EOF
  $ fe ob check
  ("invalid .fe.sexp file"
   (.fe/.fe.sexp:6:0 ("inconsistent scrutinies for .fe.sexp" (normal high))))
  [1]

Inconsistent owners.

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny high)
  > (Apply_to All_files)
  > (Owner user2)
  > (Apply_to (Files .fe.sexp))
  > EOF
  $ fe ob check
  ("invalid .fe.sexp file"
   (.fe/.fe.sexp:5:0 ("inconsistent owners for .fe.sexp" (user1 user2))))
  [1]

Multiple build projections with same scrutiny.

  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Define_build_projection normal1 ((default_scrutiny normal)))
  > (Define_build_projection normal2 ((default_scrutiny normal)))
  > EOF
  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Build_projections normal1 normal2)
  > (Apply_to All_files)
  > EOF
  $ fe ob show .fe/.fe.sexp
  ((build_projections (normal1 normal2)) (fewer_than_min_reviewers false)
   (followers ()) (is_read_by_whole_feature_reviewers true)
   (more_than_max_reviewers false) (owner user1)
   (review_obligation (All_of ())) (scrutiny_level 50) (scrutiny_name normal))

Review obligations.

  $ cat >.fe/obligations-repo.sexp <<EOF
  > EOF
  $ cat >.fe/.fe.sexp <<EOF
  > (Local
  >   (Owner user1)
  >   (Scrutiny high)
  >   (Apply_to All_files))
  > 
  > (Reviewed_by (All_of (Users user1)))
  > (Apply_to (Files .fe.sexp))
  > EOF
  $ fe ob show .fe/.fe.sexp
  ((build_projections ()) (fewer_than_min_reviewers false) (followers ())
   (is_read_by_whole_feature_reviewers true) (more_than_max_reviewers false)
   (owner user1) (review_obligation (All_of (user1))) (scrutiny_level 85)
   (scrutiny_name high))

Combination of multiple review obligations.

  $ cat >.fe/.fe.sexp <<EOF
  > (Local
  >   (Owner user1)
  >   (Scrutiny high)
  >   (Apply_to All_files))
  > 
  > (Reviewed_by (All_of (Users user1)))
  > (Apply_to (Files .fe.sexp))
  > 
  > (Reviewed_by (All_of (Users user2)))
  > (Apply_to (Files .fe.sexp))
  > EOF
  $ fe ob show .fe/.fe.sexp
  ((build_projections ()) (fewer_than_min_reviewers false) (followers ())
   (is_read_by_whole_feature_reviewers true) (more_than_max_reviewers false)
   (owner user1) (review_obligation (All_of (user1 user2))) (scrutiny_level 85)
   (scrutiny_name high))

Adding a user to obligations-repo.sexp.

  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user-only-in-obligations-repo)
  > (Scrutiny high)
  > (Apply_to All_files)
  > EOF
  $ fe ob check
  ("invalid .fe.sexp file"
   (.fe/.fe.sexp:1:0
    ("users not in obligations-global.sexp or obligations-repo.sexp"
     (user-only-in-obligations-repo))))
  [1]
  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Users user-only-in-obligations-repo)
  > EOF
  $ fe ob check

Checking a subtree.

  $ cat >.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny high)
  > (Apply_to All_files)
  > EOF
  $ hg add .fe.sexp
  $ hg com -m 'added .fe.sexp'
  $ fe ob check

Break obligations and we fail.

  $ echo >.fe.sexp
  $ fe ob check 2>/dev/null
  [1]

But if we [cd .fe], then we don't check the root [.fe.sexp], and hence succeed.

  $ cd .fe
  $ fe ob check

[fe ob report] only operates on a subtree.

  $ fe ob report | sexp query 'each (index 0)'
  .fe/.fe.sexp
  .fe/obligations-global.sexp
  .fe/obligations-repo.sexp

[fe ob show] only uses the necessary subtree for the file ...

  $ fe ob show obligations-global.sexp
  ((build_projections ()) (fewer_than_min_reviewers false) (followers ())
   (is_read_by_whole_feature_reviewers true) (more_than_max_reviewers false)
   (owner user-only-in-obligations-repo) (review_obligation (All_of ()))
   (scrutiny_level 85) (scrutiny_name high))

... even if the current working directory is elsewhere.

  $ cd $(hg root)
  $ fe ob show .fe/obligations-global.sexp
  ((build_projections ()) (fewer_than_min_reviewers false) (followers ())
   (is_read_by_whole_feature_reviewers true) (more_than_max_reviewers false)
   (owner user-only-in-obligations-repo) (review_obligation (All_of ()))
   (scrutiny_level 85) (scrutiny_name high))

The obligations check picks up the dirstate, but not necessarily the working copy.

  $ hg revert .fe.sexp
  $ mkdir subdir
  $ touch subdir/foo
  $ fe ob check
  $ hg add subdir/foo
  $ fe ob check
  ("directory missing .fe.sexp file" subdir)
  [1]
  $ cat > subdir/.fe.sexp <<EOF
  > (Owner user-only-in-obligations-repo)
  > (Scrutiny high)
  > (Apply_to All_files)
  > EOF
  $ fe ob check
  ("untracked .fe.sexp file" subdir/.fe.sexp)
  [1]
  $ hg add subdir/.fe.sexp
  $ fe ob check
  $ hg commit -m commit
  $ rm subdir/foo subdir/.fe.sexp
  $ fe ob check
  ("deleted but tracked .fe.sexp file" subdir/.fe.sexp)
  [1]
  $ hg rm subdir/foo subdir/.fe.sexp
  $ fe ob check
