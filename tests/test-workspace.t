  $ start_test

Create the repo.

  $ mkdir repo
  $ cd repo
  $ hg init
  $ echo "c1" > f1.ml
  $ hg add f1.ml
  $ hg ci -m '.'
  $ remote=$PWD

Set up some extra things needed only for workspaces.  We have to set up HOME
outside of the IRON_APPDIR because we do not want HOME to be inside a hg repo.

  $ export HOME=$(readlink -m /tmp/$IRON_APPDIR/home)
  $ rm -rf $HOME
  $ mkdir -p $HOME/workspaces

Enable workspaces by adding a clause to .ferc

  $ cat >$HOME/.ferc <<EOF
  >   (workspaces (
  >     (basedir $HOME/workspaces)
  >   ))
  > EOF

Create some features.

  $ fe create root -description 'root' -remote $remote | sed "s;$HOME;\$HOME;"
  $ fe create root/child1 -desc child1 | sed "s;$HOME;\$HOME;"
  $ fe create root/child2 -desc child2 -owner owner | sed "s;$HOME;\$HOME;"
  $ fe change root -set-owner user1

  $ tree $HOME/workspaces | sed "s;$HOME;\$HOME;"
  $HOME/workspaces
  `-- root
      |-- +clone+
      |   `-- f1.ml
      |-- +share+
      |   `-- f1.ml
      |-- child1
      |   `-- +share+
      |       `-- f1.ml
      `-- child2
          `-- +share+
              `-- f1.ml
  
  7 directories, 4 files

  $ fe workspace dir root | sed "s;$HOME;\$HOME;"
  $HOME/workspaces/root/+share+
  $ fe workspace dir root/child1 | sed "s;$HOME;\$HOME;"
  $HOME/workspaces/root/child1/+share+
  $ fe workspace dir root2
  ("no such feature" root2)
  [1]

Show the list of workspaces present in the local disk.

  $ fe workspace list
  root
  root/child1
  root/child2

Check the existence of some workspaces and clones.

  $ fe workspace exists root/child1
  true
  $ fe workspace exists root/child2
  true
  $ fe workspace exists -clone-of-root-feature-of root/the-rest-does-not/matter
  true
  $ fe workspace exists root/not-a-feature
  false
  $ fe workspace exists -clone-of-root-feature-of not-a-root/does-not-matter
  false

Check the selected repo in various cases.

  $ function repo_for_hg_operations {
  >   local dir=$1; shift
  >   (cd $dir && fe internal show-repo-for-hg-operations $@ | sed "s;$HOME;\$HOME;")
  > }

  $ repo_for_hg_operations $HOME root
  $HOME/workspaces/root/+share+

  $ repo_for_hg_operations $HOME root/child1
  $HOME/workspaces/root/child1/+share+

  $ repo_for_hg_operations $HOME root/child1 -use-clone-instead-of-share
  $HOME/workspaces/root/+clone+

  $ repo_for_hg_operations $HOME/workspaces/root/child1/+share+ root/child1
  $HOME/workspaces/root/child1/+share+

  $ repo_for_hg_operations $HOME/workspaces/root/child1/+share+ root/child2
  ("disallowed due to current workspace being different from supplied feature"
   ((current_workspace root/child1) (supplied_feature root/child2)))
  [1]

  $ hg clone -q $HOME/workspaces/root/+clone+ $HOME/my-root-clone
  $ cat >$HOME/my-root-clone/.hg/hgrc <<EOF
  > [paths]
  > default = ssh://hg//hg/root/submissions
  > EOF

  $ repo_for_hg_operations $HOME/my-root-clone root
  $HOME/my-root-clone

  $ repo_for_hg_operations $HOME/my-root-clone root/child1
  $HOME/my-root-clone

Distclean workspaces.  Test [-rec], [-exclude], [-dry-run] and exclusion via [.ferc]

  $ CHILD2=$(fe workspace dir root/child2)

  $ ( cd $CHILD2 \
  >     && echo "foo" > .hgignore \
  >     && hg add .hgignore \
  >     && hg -q commit -m "hgignore" \
  >     && hg -q push \
  > )

  $ touch $CHILD2/foo
  $ hg status --cwd $CHILD2
  $ fe workspace distclean root/child2
  $ test -f $CHILD2/foo
  [1]

  $ touch $CHILD2/foo
  $ fe workspace distclean root -rec
  $ test -f $CHILD2/foo
  [1]

  $ touch $CHILD2/foo
  $ fe workspace distclean root -rec -dry-run
  Proceeding will distclean these workspaces:
    root
    root/child1
    root/child2
  $ test -f $CHILD2/foo

  $ touch $CHILD2/foo
  $ echo '' | fe workspace distclean root -rec -interactive true
  Proceeding will distclean these workspaces:
    root
    root/child1
    root/child2
  Proceed?  [y/N]: Aborted.
  
  $ test -f $CHILD2/foo

  $ echo 'y' | fe workspace distclean root -rec -interactive true > /dev/null
  $ test -f $CHILD2/foo
  [1]

  $ touch $CHILD2/foo
  $ fe workspace distclean root -rec -exclude root/child2
  $ test -f $CHILD2/foo

  $ touch $CHILD2/foo
  $ cat >>$HOME/.ferc <<EOF
  > (workspaces (
  >   (do_not_distclean (
  >     root/child2
  >   ))
  > ))
  > EOF
  $ fe workspace distclean root -rec
  $ test -f $CHILD2/foo
  $ rm -f $CHILD2/foo

Delete a workspace.

  $ fe workspace list
  root
  root/child1
  root/child2
  $ fe workspace unclean list

  $ touch $(fe workspace dir root)/FILE
  $ fe workspace delete root \
  >   |& matches "failed to delete workspace.*unclean workspace.*uncommitted changes"
  [1]
  $ fe workspace unclean list -name-only
  root
  $ fe workspace unclean list | sed "s;$HOSTNAME;\$HOSTNAME;"
  Unclean workspaces on $HOSTNAME:
  |-------------------------------|
  | feature | reason              |
  |---------+---------------------|
  | root    | uncommitted changes |
  |-------------------------------|
  
  $ fe workspace unclean check -all
  (errors (((feature_path root) (reason (Uncommitted_changes)))))
  [1]
  $ rm -f $(fe workspace dir root)/FILE
  $ fe workspace unclean list

  $ fe workspace delete root
  $ tree $HOME/workspaces | sed "s;$HOME;\$HOME;"
  $HOME/workspaces
  `-- root
      |-- +clone+
      |   `-- f1.ml
      |-- child1
      |   `-- +share+
      |       `-- f1.ml
      `-- child2
          `-- +share+
              `-- f1.ml
  
  6 directories, 3 files

  $ fe workspace dir root
  ("you don't have a workspace for" root)
  [1]

Rename a feature.

  $ fe create root/child1/greatchild1 -description 'greatchild1'
  $ fe rename root/child1 root/child3 | sed "s;$HOME;\$HOME;"
  $ tree $HOME/workspaces | sed "s;$HOME;\$HOME;"
  $HOME/workspaces
  `-- root
      |-- +clone+
      |   `-- f1.ml
      |-- child2
      |   `-- +share+
      |       `-- f1.ml
      `-- child3
          |-- +share+
          |   `-- f1.ml
          `-- greatchild1
              `-- +share+
                  `-- f1.ml
  
  8 directories, 4 files

Compress a feature.

  $ fe compress root/child3
  $ tree $HOME/workspaces | sed "s;$HOME;\$HOME;"
  $HOME/workspaces
  `-- root
      |-- +clone+
      |   `-- f1.ml
      |-- child2
      |   `-- +share+
      |       `-- f1.ml
      `-- greatchild1
          `-- +share+
              `-- f1.ml
  
  6 directories, 3 files

Unclean workspaces and interaction with [fe todo] and [fe show].

  $ cat >>$HOME/.ferc <<EOF
  > (workspaces (
  >   (unclean_workspaces_detection_is_enabled false)
  > ))
  > EOF

  $ fe workspace unclean update-server root/child2
  Unclean workspaces detection is not enabled.
  Consider enabling the functionality via your [.ferc], or supply -do-nothing-if-not-enabled.
  [1]

  $ fe workspace unclean update-server root/child2 -do-nothing-if-not-enabled \
  >   -interactive true
  Unclean workspaces detection is not enabled, and the switch -do-nothing-if-not-enabled was supplied.
  Exiting with code 0

Be silent if not interactive.

  $ fe workspace unclean update-server root/child2 -do-nothing-if-not-enabled \
  >   -interactive false

  $ cat >>$HOME/.ferc <<EOF
  > (workspaces (
  >   (unclean_workspaces_detection_is_enabled true)
  > ))
  > EOF

  $ touch $(fe workspace dir root/child2)/FILE
  $ fe workspace unclean update-server root/child2
  $ fe todo -unclean-workspaces | sed "s;$HOSTNAME;\$HOSTNAME;"
  Unclean workspaces on $HOSTNAME:
  |--------------------------------|
  | feature  | reason              |
  |----------+---------------------|
  | root     |                     |
  |   child2 | uncommitted changes |
  |--------------------------------|

  $ fe show root/child2 -omit-attribute-table
  root/child2
  ===========
  child2
  
  |----------------------------------------------|
  | unclean workspaces     | reason              |
  |------------------------+---------------------|
  | unix-login-for-testing | uncommitted changes |
  |----------------------------------------------|

Check that this is persisted.

  $ fe-server stop
  $ fe-server start

  $ fe todo -unclean-workspaces | sed "s;$HOSTNAME;\$HOSTNAME;"
  Unclean workspaces on $HOSTNAME:
  |--------------------------------|
  | feature  | reason              |
  |----------+---------------------|
  | root     |                     |
  |   child2 | uncommitted changes |
  |--------------------------------|

  $ fe show root/child2 -omit-attribute-table
  root/child2
  ===========
  child2
  
  |----------------------------------------------|
  | unclean workspaces     | reason              |
  |------------------------+---------------------|
  | unix-login-for-testing | uncommitted changes |
  |----------------------------------------------|

Check that users with unclean workspaces are included in [fe remind].

  $ feature_to_server root/child2 -fake-valid-obligations
  $ echo n | fe remind root/child2 -interactive true
  Sending mail
  
  ------
  Subject: reminder for Iron feature root/child2
  
  |----------------------------------------------|
  | unclean workspaces     | reason              |
  |------------------------+---------------------|
  | unix-login-for-testing | uncommitted changes |
  |----------------------------------------------|
  
  root/child2
  ===========
  child2
  ------
  to the following users: 
    owner
    unix-login-for-testing
  
  Send mail? [y/n/e/?]: Aborted

  $ rm $(fe workspace dir root/child2)/FILE

When running post commit hooks, this refreshes the server state.

  $ (cd $(fe workspace dir root/child2) && fe tools hg-hooks post-commit -fg &> /dev/null)

  $ fe todo -unclean-workspaces | sed "s;$HOSTNAME;\$HOSTNAME;"
  $ fe show root/child2 -omit-attribute-table
  root/child2
  ===========
  child2
  
  |---------------------|
  | user       | review |
  |------------+--------|
  | file-owner |      2 |
  | owner      |      2 |
  |---------------------|

  $ fe remind root/child2 -just-print-recipients-and-exit
  owner

Check workspace invariants.

  $ fe internal invariant check-workspaces
  $ hg pull   -q -r root --cwd $(fe workspace dir root/child2)
  $ hg update -q -r root --cwd $(fe workspace dir root/child2)
  $ fe internal invariant check-workspaces \
  >    |& matches '.*root/child2/+share+.*unexpected current bookmark.*'
  [1]
  $ hg update -q -r root/child2  --cwd $(fe workspace dir root/child2)
  $ fe internal invariant check-workspaces

Archive a feature.

  $ fe archive root/child2 -for owner
  $ tree $HOME/workspaces | sed "s;$HOME;\$HOME;"
  $HOME/workspaces
  `-- root
      |-- +clone+
      |   `-- f1.ml
      `-- greatchild1
          `-- +share+
              `-- f1.ml
  
  4 directories, 2 files

Attempt to access a non-existent feature and get a normal Iron error
rather than a share error.

  $ fe review nonexistent-feature
  ("no such feature" nonexistent-feature)
  [1]
  $ fe review root/nonexistent-feature
  ("no such feature" root/nonexistent-feature)
  [1]

Test options related to users' todo.

  $ fe create root/foo -owner user1 -description root/foo
  $ (cd $(fe workspace dir root/foo) \
  >   && echo "# $CR user1 for user2: blah" >file \
  >   && hg -q add file && hg -q com -m 'added CR' \
  >   && feature_to_server root/foo -fake-valid-obligations \
  >   && hg push -q \
  > )

  $ fe todo -for user1 -crs-and-review-names
  $ fe todo -for user1 -owned-by-me-names
  root
  root/foo

  $ fe todo -for user2 -crs-and-review-names
  root/foo
  $ fe todo -for user2 -owned-by-me-names

  $ fe workspace delete root/foo
  $ fe workspace list
  root/greatchild1
  $ fe workspace create -features-owned-by-me -for user2
  $ fe workspace list
  root/greatchild1
  $ fe workspace create -features-assigned-to-me -for user2
  $ fe workspace list
  root/foo
  root/greatchild1

  $ fe workspace delete root/greatchild1
  $ fe workspace list
  root/foo

  $ fe workspace create root/greatchild1 \
  >  -clone-of-root-feature-of root/the-rest-does-not/matter
  $ fe workspace list
  root/foo
  root/greatchild1

  $ fe workspace distclean -dry-run -all
  Proceeding will distclean these workspaces:
    root/foo
    root/greatchild1

  $ fe workspace distclean -for user2 -dry-run -all -exclude-features-assigned-to-me
  Proceeding will distclean this workspace:
    root/greatchild1

  $ fe workspace distclean -for user1 -dry-run -all -exclude-features-owned-by-me
  Proceeding will distclean this workspace:
    root/greatchild1

Create spare shares

  $ fe workspace create -features-in-my-todo -num-spares 5
  $ ls $HOME/workspaces/root/+clone+/.hg/spare-shares
  [a-f0-9-]{36} (re)
  [a-f0-9-]{36} (re)
  [a-f0-9-]{36} (re)
  [a-f0-9-]{36} (re)
  [a-f0-9-]{36} (re)
  staging

Use a spare share

  $ IRON_OPTIONS='((verbose (Workspaces)))' fe create root/child4 -desc child4 |& matches 'succeeded in claiming share'
  $ ls $HOME/workspaces/root/+clone+/.hg/spare-shares
  [a-f0-9-]{36} (re)
  [a-f0-9-]{36} (re)
  [a-f0-9-]{36} (re)
  [a-f0-9-]{36} (re)
  staging

Now force the spare share count back up to 5

  $ fe workspace create -features-in-my-todo -num-spares 5
  $ ls $HOME/workspaces/root/+clone+/.hg/spare-shares
  [a-f0-9-]{36} (re)
  [a-f0-9-]{36} (re)
  [a-f0-9-]{36} (re)
  [a-f0-9-]{36} (re)
  [a-f0-9-]{36} (re)
  staging

Testing creation and other operations for scaffold repos.

Create a scaffold structure similar to jane/scaffoo.

  $ hg init jane
  $ touch jane/OMakeroot jane/this-is-jane-root
  $ hg -q addremove      --cwd jane
  $ hg -q commit -m init --cwd jane
  $ JANE=$(hg log -r . --template="{node|short}" --cwd jane)
  $ (cd jane \
  >   && fe create jane -d jane -permanent -remote-repo-path "$PWD" \
  >   && feature_to_server jane -fake-valid \
  > )
  $ echo "change" > jane/this-is-jane-root
  $ hg -q addremove        --cwd $PWD/jane
  $ hg -q commit -m change --cwd $PWD/jane
  $ (cd jane \
  >   && feature_to_server jane -fake-valid \
  > )

  $ hg init scaffoo
  $ cat > scaffoo/scaffold.sexp <<EOF
  > ((repo $PWD/jane)
  > (id $JANE)
  > (others (((dir scaffoo/) (repo self)))))
  > EOF
  $ hg -q addremove      --cwd scaffoo
  $ hg -q commit -m init --cwd scaffoo
  $ (cd scaffoo \
  >   && fe create scaffoo -d scaffoo -permanent -remote-repo-path "$PWD" \
  >   && feature_to_server scaffoo -fake-valid \
  > )

Create a scaffoo feature and add a change to it.

  $ fe create scaffoo/child -d scaffoo/child
  $ touch $(fe workspace dir scaffoo/child)/foo
  $ hg -q addremove    --cwd $(fe workspace dir scaffoo/child)
  $ hg -q commit -m child --cwd $(fe workspace dir scaffoo/child)
  $ (cd $(fe workspace dir scaffoo/child) && feature_to_server scaffoo/child -fake-valid)
  $ hg -q push --cwd $(fe workspace dir scaffoo/child)

Verify the layout of enclosing scaffolded workspace.

  $ tree $HOME/workspaces/scaffoo | sed "s;$HOME;\$HOME;"
  $HOME/workspaces/scaffoo
  |-- +clone+
  |   `-- scaffold.sexp
  |-- +share+
  |   |-- OMakeroot
  |   |-- scaffoo
  |   |   `-- scaffold.sexp
  |   `-- this-is-jane-root
  `-- child
      `-- +share+
          |-- OMakeroot
          |-- scaffoo
          |   |-- foo
          |   `-- scaffold.sexp
          `-- this-is-jane-root
  
  6 directories, 8 files

Update the hgrc to please the heuristic used by Iron to let a command happen in
the local repo as opposed to select a workspace.

  $ cat >$HOME/workspaces/scaffoo/+share+/.hg/hgrc <<EOF
  > [paths]
  > default = ssh://hg//hg/jane/fake-submissions
  > EOF
  $ cat >$HOME/workspaces/scaffoo/+clone+/.hg/hgrc <<EOF
  > [paths]
  > default = ssh://hg//hg/scaffoo/fake-submissions
  > EOF

Check repo_for_hg_operations.

  $ repo_for_hg_operations $HOME/workspaces/scaffoo/+share+ jane
  $HOME/workspaces/scaffoo/+share+
  $ repo_for_hg_operations $HOME/workspaces/scaffoo/+share+ scaffoo
  $HOME/workspaces/scaffoo/+share+/scaffoo
  $ repo_for_hg_operations $HOME/workspaces/scaffoo/+share+ scaffoo/child
  ("disallowed due to current workspace being different from supplied feature"
   ((current_workspace scaffoo) (supplied_feature scaffoo/child)))
  [1]
  $ repo_for_hg_operations $HOME/workspaces/scaffoo/+share+/scaffoo jane
  ("disallowed due to current workspace being different from supplied feature"
   ((current_workspace scaffoo) (supplied_feature jane)))
  [1]
  $ repo_for_hg_operations $HOME/workspaces/scaffoo/+share+/scaffoo scaffoo
  $HOME/workspaces/scaffoo/+share+/scaffoo
  $ repo_for_hg_operations $HOME/workspaces/scaffoo/+share+/scaffoo scaffoo/child
  ("disallowed due to current workspace being different from supplied feature"
   ((current_workspace scaffoo) (supplied_feature scaffoo/child)))
  [1]

Check that one can update in the clone (isolated scaffold repo).
This updates to the right revision and the right bookmark is active.

  $ (cd $HOME/workspaces/scaffoo/+clone+ && fe update scaffoo/child)
  $ A=$(fe show -tip scaffoo/child)
  $ B=$(hg log -r . --template="{node}" --cwd $HOME/workspaces/scaffoo/+clone+)
  $ test $A = $B
  $ hg id --cwd $HOME/workspaces/scaffoo/+clone+
  * tip scaffoo/child (glob)

Check the update satellite logic.

1) Update the enclosing repo to a wrong revision.

  $ hg -q pull $PWD/jane --cwd $HOME/workspaces/scaffoo/+share+
  $ hg -q update -r 1    --cwd $HOME/workspaces/scaffoo/+share+
  $ hg log -r . --template="{node}\n" --cwd $HOME/workspaces/scaffoo/+share+ \
  >   | matches "$JANE"
  * (glob)

2) Run fe update in the workspace.

  $ fe update scaffoo

3) The enclosing repo is now at the right revision (referenced in the scaffold file).

  $ hg log -r . --template="{node|short}\n" --cwd $HOME/workspaces/scaffoo/+share+ \
  >   | sed "s;$JANE;\$JANE;"
  $JANE

Check workspaces operations on release.

  $ fe create jane/a -desc a
  $ fe create jane/a/child -desc child

  $ fe enable-review jane/a
  $ fe enable-review jane/a/child

  $ (cd $(fe workspace dir jane/a) && feature_to_server jane/a -fake-valid)
  $ (cd $(fe workspace dir jane/a/child) && feature_to_server jane/a/child -fake-valid)

  $ fe second jane/a -even-though-owner -even-though-empty
  $ fe second jane/a/child -even-though-owner -even-though-empty

  $ fe change -set-release-process direct jane/a

  $ echo "foo" >$(fe workspace dir jane/a/child)/foo
  $ hg -q addremove       --cwd $(fe workspace dir jane/a/child)
  $ hg -q commit -m child --cwd $(fe workspace dir jane/a/child)
  $ (cd $(fe workspace dir jane/a/child) && feature_to_server jane/a/child -fake-valid)
  $ hg -q push --cwd $(fe workspace dir jane/a/child)
  $ (cd $(fe workspace dir jane/a) && fe update jane/a && feature_to_server jane/a -fake-valid)

  $ IRON_USER=file-owner fe internal mark-fully-reviewed jane/a/child
  $ fe internal mark-fully-reviewed jane/a/child

  $ tree $HOME/workspaces/jane/a | sed "s;$HOME;\$HOME;"
  $HOME/workspaces/jane/a
  |-- +share+
  |   |-- OMakeroot
  |   `-- this-is-jane-root
  `-- child
      `-- +share+
          |-- OMakeroot
          |-- foo
          `-- this-is-jane-root
  
  3 directories, 5 files

  $ RELEASED_TIP=$(fe show jane/a/child -tip)
  $ fe release jane/a/child

1) The workspace of the released feature is gone.

  $ tree $HOME/workspaces/jane/a | sed "s;$HOME;\$HOME;"
  $HOME/workspaces/jane/a
  `-- +share+
      |-- OMakeroot
      `-- this-is-jane-root
  
  1 directory, 2 files

2) The workspace of the parent is up to date.


  $ (cd $(fe workspace dir jane/a) && hg log -r . --template="{node}\n") \
  >   | sed "s;$RELEASED_TIP;\$RELEASED_TIP;" | grep -q '$RELEASED_TIP'
  [1]

Clean up the "home" directory.

  $ rm -rf $HOME
