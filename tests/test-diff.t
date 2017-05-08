  $ start_test

Setup repo with these attributes:

| file            | scrutiny | reviewed by |
|-----------------+----------+-------------|
| file-user1      | normal   | user1       |
| file-unreviewed | normal   |             |
| file-ignored    | ignored  |             |

  $ setup_repo_and_root file-user1 file-unreviewed file-ignored
  $ mkdir .fe
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Define_scrutiny normal  ((level 50) (description "normal")))
  > (Define_scrutiny ignored
  >   ((level  0)
  >    (description "ignored")
  >    (read_by_whole_feature_reviewers false)))
  > (Users unix-login-for-testing user1)
  > EOF
  $ touch .fe/obligations-repo.sexp
  $ cat >.fe/.fe.sexp <<EOF
  > (Owner unix-login-for-testing)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF
  $ cat >.fe.sexp <<EOF
  > (Owner unix-login-for-testing)
  > (Scrutiny ignored)
  > (Apply_to (Files file-ignored))
  > (Scrutiny normal)
  > (Apply_to (Files .fe.sexp file-unreviewed))
  > (Reviewed_by (All_of (Users user1)))
  > (Apply_to (Files file-user1))
  > EOF
  $ hg add -q .
  $ hg com -m 'added files'
  $ fe change -set-base $(hg tip --template={node})
  $ fe change -remove-whole-feature-reviewers unix-login-for-testing
  $ for f in file-*; do echo "change" >"$f"; done
  $ hg commit -m "change"
  $ feature_to_server root

The default [-for] is to show the whole feature.

  $ fe diff -summary
  |-------------------------------|
  | file            | op  | lines |
  |-----------------+-----+-------|
  | file-unreviewed | mod |     2 |
  | file-user1      | mod |     2 |
  |-------------------------------|

[-for all] is the same as the default.

  $ fe diff -summary -for all
  |-------------------------------|
  | file            | op  | lines |
  |-----------------+-----+-------|
  | file-unreviewed | mod |     2 |
  | file-user1      | mod |     2 |
  |-------------------------------|

Adding [-even-ignored] includes ignored files.

  $ fe diff -summary -even-ignored
  |-------------------------------|
  | file            | op  | lines |
  |-----------------+-----+-------|
  | file-ignored    | mod |     2 |
  | file-unreviewed | mod |     2 |
  | file-user1      | mod |     2 |
  |-------------------------------|

[-for user1] shows only user1's diff.

  $ fe diff -summary -for user1
  |--------------------------|
  | file       | op  | lines |
  |------------+-----+-------|
  | file-user1 | mod |     2 |
  |--------------------------|

  $ fe diff -summary -for user1 -even-ignored
  Cannot use -even-ignored and -for USER.
  [1]

user2 has no diff.

  $ fe diff -summary -for user2 |& matches "no CRs or review work"
  [1]

A diff that hangs if [fe] doesn't play nice with pipelines.  We set
TMPDIR so we can compare the directory before and after, and make sure
that we clean up the fe_cat temporary directory.

  $ before=$(ls -1)
  $ ( sleep 1; TMPDIR=$(pwd) fe diff root ) | exit
  $ after=$(ls -1)
  $ diff -u <(echo "$before") <(echo "$after")

Failure if [-file] file doesn't exist.

  $ fe diff root -summary -file no-such-file |& matches "file selections not found"
  [1]

The actual diff, not a summary.

  $ THE_DIFF=$(fe diff -even-ignored | fe internal remove-color)
  $ echo "${THE_DIFF}"
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file-ignored @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny ignored
  base 3e74663d5559 | tip 357576f64f7a
  _
  | @@@@@@@@ Hunk 1/3 @@@@@@@@
  | @@@@@@@@ base 1,2 tip 1,2 @@@@@@@@
  | -|file-ignored
  | +|change
  |_
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file-unreviewed @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  _
  | @@@@@@@@ Hunk 2/3 @@@@@@@@
  | @@@@@@@@ base 1,2 tip 1,2 @@@@@@@@
  | -|file-unreviewed
  | +|change
  |_
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file-user1 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny normal
  _
  | @@@@@@@@ Hunk 3/3 @@@@@@@@
  | @@@@@@@@ base 1,2 tip 1,2 @@@@@@@@
  | -|file-user1
  | +|change
  |_

Use [-base] and [-tip].

  $ fe diff -base $(fe show -base) -tip $(fe show -base)

  $ diff \
  >  <(fe diff -summary -even-ignored) \
  >  <(fe diff -summary -base $(fe show -base) -tip $(fe show -tip))

  $ diff -U 0 \
  >  <(fe diff -even-ignored                               | fe internal remove-color) \
  >  <(fe diff -base $(fe show -base) -tip $(fe show -tip) | fe internal remove-color) \
  >  | sed -r 1,2d
  @@ -3 +3 @@
  -scrutiny ignored
  +scrutiny scrutiny
  @@ -13 +13 @@
  -scrutiny normal
  +scrutiny scrutiny
  @@ -22 +22 @@
  -scrutiny normal
  +scrutiny scrutiny
  [1]

Check that one can still get the diff of an archived feature.

  $ fe change -set-is-permanent false root
  $ fe archive root

  $ fe diff -summary root -archived
  |-------------------------------|
  | file            | op  | lines |
  |-----------------+-----+-------|
  | file-unreviewed | mod |     2 |
  | file-user1      | mod |     2 |
  |-------------------------------|

  $ diff \
  >  <(echo "${THE_DIFF}") \
  >  <(fe diff -even-ignored root -archived | fe internal remove-color)
