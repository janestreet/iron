Start test.

  $ start_test

Create .fe files.

  $ setup_repo_and_root .fe/.fe.sexp
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Users user1)
  > (Define_scrutiny normal
  >   ((level 0)
  >    (description "normal scrutiny description")
  >    (min_file_reviewers 0)
  >    (max_file_reviewers 10)
  >   ))
  > (Define_tags
  >    database
  > )
  > EOF
  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Define_tags
  >   post-trade
  >   unused
  > )
  > (Define_tags
  >   database
  >   post-trade
  > )
  > EOF
  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Reviewed_by (All_of (Users user1)))
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF
  $ mkdir -p post-trade/database
  $ cat >.fe.sexp <<EOF
  > (Owner user1)
  > (Reviewed_by (All_of (Users user1)))
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF
  $ cat >post-trade/.fe.sexp <<EOF
  > (Tags post-trade)
  > (Owner user1)
  > (Reviewed_by (All_of (Users user1)))
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF
  $ cat >post-trade/database/.fe.sexp <<EOF
  > (Tags post-trade database)
  > (Owner user1)
  > (Reviewed_by (All_of (Users user1)))
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF
  $ cat >post-trade/README.txt <<EOF
  > Some post-trade stuff.
  > The file is called README in the hope that some people will read it.
  > post-trade's blessing is given if it is not appreciably worse than before.
  > EOF
  $ cat >post-trade/database/README.org <<EOF
  > Some database stuff.
  > * Using org syntax
  > ** Why org ?
  > Because org is so cool
  > EOF
  $ hg add -q
  $ hg com -m 'added files'


It is an error to define a tag more that once.

  $ fe projects list-tags
  ($TESTTMP/repo/.fe/obligations-repo.sexp:5:0
   ("multiply defined tag" post-trade))
  [1]

  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Define_tags
  >   database
  >   post-trade
  >   unused
  > )
  > EOF

  $ fe projects list-tags
  ($TESTTMP/repo/.fe/obligations-repo.sexp:0:0
   ("remove tags from .fe/obligations-repo.sexp that are also in .fe/obligations-global.sexp (which comes from the scaffolded repo)"
    (database)))
  [1]

  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Define_tags
  >   post-trade
  >   unused
  > )
  > EOF

Show tags.

  $ fe projects list-tags
  database
  post-trade
  unused

Unused tags.

  $ fe obligations check
  ("unused tags -- please remove from obligations-repo.sexp" (unused))
  [1]

Remove the unused projection.

  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Define_tags
  >   post-trade
  > )
  > EOF
  $ fe obligations check

  $ hg commit -m "fix obligations"
  $ fe change -set-base $(hg tip --template={node})
  $ feature_to_server root

Do no restrict to any tag, get all the projects with their header.

  $ fe projects list
  post-trade           Some post-trade stuff.
  post-trade/database  Some database stuff.

Asking for non-existent tag is an error.

  $ fe projects list -tag zzz
  ("undefined tags" (zzz))
  [1]

Find the projects with a given tag.

  $ fe projects list -directory-only -tag post-trade
  post-trade
  post-trade/database

  $ fe projects list -directory-only -tag database
  post-trade/database

  $ fe projects list -directory-only -tag '(and post-trade (not database))'
  post-trade

Check that completion works on tags blang.

  $ completion-test fe projects list -tag post-t
  post-trade

  $ completion-test fe projects list -tag '(and post-t'
  (and post-trade

One can request to see the entire READMEs.

  $ fe projects list -show-readme -tag post-trade
  post-trade/README.txt: (tags post-trade)
  ----------------------
    Some post-trade stuff.
    The file is called README in the hope that some people will read it.
    post-trade's blessing is given if it is not appreciably worse than before.
  
  post-trade/database/README.org: (tags database, post-trade)
  -------------------------------
    Some database stuff.
    * Using org syntax
    ** Why org ?
    Because org is so cool
  
One can search for patterns inside tags or contents.

  $ fe project search 'not appreciably worse than before'
  post-trade/README.txt

  $ fe project search 'stuff'
  post-trade/README.txt
  post-trade/database/README.org

Add a new tag, change the tag of a file.

  $ cat >.fe/obligations-repo.sexp <<EOF
  > (Define_tags
  >   machine-learning
  >   post-trade
  > )
  > EOF

  $ cat >post-trade/.fe.sexp <<EOF
  > (Tags machine-learning)
  > (Owner user1)
  > (Reviewed_by (All_of (Users user1)))
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF

  $ hg commit -m "change some tags"
  $ feature_to_server root

The modification of the file are shown because these sexp files are reviewed.
However, there is no associated attribute review for the tag changes.  This
would require a change in the stable type [Attribute_file.t] and it is not clear
at this point that we really want/need this.

  $ fe session diff -for user1 | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@ .fe/obligations-repo.sexp @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  base * | tip * (glob)
  _
  | @@@@@@@@ Hunk 1/2 @@@@@@@@
  | @@@@@@@@ base 1,4 tip 1,5 @@@@@@@@
  |   (Define_tags
  | +|  machine-learning
  |     post-trade
  |   )
  |_
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ post-trade/.fe.sexp @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  _
  | @@@@@@@@ Hunk 2/2 @@@@@@@@
  | @@@@@@@@ base 1,6 tip 1,6 @@@@@@@@
  | -|(Tags post-trade)
  | +|(Tags machine-learning)
  |   (Owner user1)
  |   (Reviewed_by (All_of (Users user1)))
  |   (Scrutiny normal)
  |   (Apply_to All_files)
  |_
