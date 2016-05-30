  $ start_test

Setup root feature.

  $ setup_repo_and_root file
  $ spec=.projections/spec.txt
  $ mkdir .projections
  $ touch $spec; hg add $spec; hg com -m "added $spec"
  $ feature_to_server root -fake-valid

Setup a child feature that changes spec.txt.

  $ fe create root/child -d child
  $ echo 'change' >$spec; hg com -m "changed $spec"
  $ feature_to_server root/child -fake-valid

Extend root to switch to .fe.sexp, i.e. remove spec.txt.

  $ fe up root
  $ hg rm $spec; hg com -q -m "removed $spec"
  $ feature_to_server root -fake-valid

Now the old tip has spec.txt and the new base doesn't.

  $ function has_spec {
  >    hg locate -r $1 'path:.projections/spec.txt' >/dev/null
  > }
  $ has_spec root/child
  $ has_spec root 
  [1]

Rebasing works, and removes spec.txt.

  $ fe rebase root/child -interactive true
  Checking cleanliness of local repo ... done.
  Updating local repo to root/child ... done.
  Removing .projections due to rebase across switch-to-dot-fe.
  Merging with f47c00bf805d.
  0 files updated, 0 files merged, 0 files removed, 0 files unresolved
  (branch merge, don't forget to commit)
  $ has_spec root/child
  [1]
