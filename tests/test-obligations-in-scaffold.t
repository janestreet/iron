  $ start_test


Kerberos authentication doesn't work for localhost because it matches on names, so
we resolve localhost ourselves.

  $ host=$(hostname -s)

Create a repo with valid obligations.

  $ start="$PWD"
  $ copy_sample_repo two-users ./repo
  $ cd repo
  $ hg init
  $ hg add -q .
  $ hg commit -m 0
  $ hg st
  $ rev=$(hg tip --template={node})
  $ hg status
  $ fe ob check

  $ function scaffold-repo {
  >   copy_sample_repo two-users $1
  >   cd $1
  >   hg init
  >   rm .fe/obligations-global.sexp
  >   hg add a b .fe/obligations-repo.sexp .fe/.fe.sexp .fe.sexp
  >   hg commit -m 0
  > }

Iron in a nested repo looks in the outer tree.

  $ scaffold-repo nested-scaffold
  $ fe ob check

The filesystem takes precedence over the scaffold.sexp.

  $ echo '(syntax error' > scaffold.sexp
  $ fe ob check

In a scaffold repo that is not nested, Iron reads the scaffold file:
We have to go in /tmp, because when run from jenga, we run inside of
jane, so Iron finds the .fe/obligations-global.sexp of jane itself.

  $ temp_dir="$(mktemp -d --tmpdir=/tmp fe_obXXXXXX)"
  $ trap 'exit_trap; rm -rf "$temp_dir"' EXIT
  $ scaffold-repo "$temp_dir"/scaffold-repo

  $ cat > scaffold.sexp <<EOF
  > ((repo $start/repo)
  >  (id 0)
  >  (others (((dir friend) (repo self)))))
  > EOF
  $ fe ob check

  $ cat > scaffold.sexp <<EOF
  > ((repo ssh://$host/$start/repo)
  >  (id 0)
  >  (others (((dir friend) (repo self)))))
  > EOF
  $ fe ob check

  $ cat > scaffold.sexp <<EOF
  > ((repo ssh://$host//no-such-repo)
  >  (id 0))
  > EOF
  $ fe ob check |& matches "Error loading .fe/obligations-global.sexp via scaffold.sexp.*No such file or directory: '/no-such-repo'"
  [1]

Scaffold_requires_global_tag_or_rev_hash failure.

  $ cat > scaffold.sexp <<EOF
  > ((repo $start/repo)
  >  (id 0)
  >  (others (((dir friend) (repo self)))))
  > EOF
  $ echo 'Scaffold_requires_global_tag_or_rev_hash' >.fe/obligations-repo.sexp
  $ fe ob check \
  >     |& matches "scaffold file must use either a global tag or a 40-char revision hash"
  [1]

Scaffold_requires_global_tag_or_rev_hash success.

  $ cat > scaffold.sexp <<EOF
  > ((repo $start/repo)
  >  (id $rev)
  >  (others (((dir friend) (repo self)))))
  > EOF
  $ fe ob check
