# Use special variable to force the hydra worker to check consistency of
# incremental computation.

  $ export IRON_FUNCTIONAL_TESTING_INCREMENTAL_REV_FACTS=1

  $ function feature_to_server_ {
  >   feature_to_server "$@" \
  >     | matches 'Check equality of tip facts computed from scratch and incrementally...'
  > }

# Between changes, advance the base to the tip since the incremental computation
# is done only between base and tip.

  $ function advance_child_base {
  >   fe change root/child -set-base $(fe show root/child -tip)
  >   feature_to_server_ root/child
  > }

Start test.

  $ start_test

Setup repo.

  $ setup_repo_and_root .fe.sexp file1 file2 file3
  $ mkdir .fe
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Define_scrutiny normal  ((level 50) (description "normal")))
  > (Define_scrutiny ignore  ((level  0) (description "ignore")))
  > (Users unix-login-for-testing user1 user2)
  > EOF
  $ touch .fe/obligations-repo.sexp
  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny ignore)
  > (Apply_to All_files)
  > EOF
  $ cat >.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Reviewed_by (All_of (Users user1 user2)))
  > (Apply_to All_files)
  > EOF
  $ for file in file{1,2,3} ; do \
  > cat >$file <<EOF
  > # $CR-soon user1: this is a cr-soon in $file
  > EOF
  > done
  $ hg add -q .
  $ hg com -m 'added files'
  $ fe enable
  $ fe change -set-base root
  $ feature_to_server_ root
  $ fe ob check
  $ fe second -even-though-owner -even-though-empty

Create child.

  $ fe create root/child -d child
  $ fe enable-review
  $ feature_to_server_ root/child

  $ fe crs -soon root -for user1
  file1:1:1:
    CR-soon user1: this is a cr-soon in file1
  
  file2:1:1:
    CR-soon user1: this is a cr-soon in file2
  
  file3:1:1:
    CR-soon user1: this is a cr-soon in file3

Add a cr in each file.

  $ for file in file{1,2,3} ; do \
  > cat >>$file <<EOF
  > # $CR user1: this is a cr
  > EOF
  > done
  $ hg com -m 'added crs'
  $ feature_to_server_ root/child

Modify the cr in one file, add an other assignee.

  $ advance_child_base

  $ (hg cat file1 -r root ; cat )>file1 <<EOF
  > # $CR user1 for user2: this is a cr
  > EOF
  $ hg com -m 'update cr'
  $ feature_to_server_ root/child

Move a file.

  $ advance_child_base

  $ hg mv file2 file2_renamed
  $ hg commit -m 'rename'
  $ feature_to_server_ root/child

Remove a file.

  $ advance_child_base

  $ hg rm file2_renamed
  $ hg commit -m 'remove'
  $ feature_to_server_ root/child

Add a file.

  $ advance_child_base

  $ cat >>file4 <<EOF
  > # $CR-soon user1: this is a cr-soon in file4
  > # $CR user1: this is a cr
  > EOF
  $ hg add file4
  $ hg commit -m 'added file'
  $ feature_to_server_ root/child

Change of file owner.

  $ advance_child_base

  $ cat >.fe.sexp <<EOF
  > (Owner user2)
  > (Scrutiny normal)
  > (Reviewed_by (All_of (Users user1 user2)))
  > (Apply_to All_files)
  > EOF
  $ hg com -m 'changed owner'
  $ feature_to_server_ root/child
