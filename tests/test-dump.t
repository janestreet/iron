Start test.

  $ start_test

Create hg repo.

  $ mkdir repo
  $ cd repo
  $ remote=$(pwd)
  $ hg init
  $ touch a
  $ hg add a
  $ hg com -m 'a'

Create feature.

  $ f=root
  $ fe create $f -description "$f" -remote-repo-path $remote
  $ fe internal dump state >/dev/null
  $ fe internal dump feature $f >/dev/null
  $ fe internal dump review-manager $f -for no-such-owner |& matches "no CRs or review work"
  [1]

Add file.

  $ owner=$(fe show $f -owners | sexp query each | head -1)
  $ file=file
  $ cat >$file <<EOF
  > (* $CR $owner for $owner: *)
  > EOF
  $ hg add $file
  $ hg com -m "$file"

Tell server and dump review manager.

  $ feature_to_server $f -fake-valid-obligations
  $ fe internal dump review-manager $f -for $owner >/dev/null

Version_util stuff.

  $ fe admin server build-info >/dev/null
  $ fe admin server version >/dev/null
