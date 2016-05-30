
  $ start_test


  $ fe tools feature-table-of-csv <<EOF
  > feature,megabytes
  > root/patdiff,1046
  > root/patdiff/stuff,1003
  > other-root/child1,512
  > other-root,517
  > other-root/shared/child,512
  > other-root/shared,1803
  > root/emacs,455
  > root/emacs/dev,1145
  > root/emacs/dev/a,460
  > root,5244
  > root/emacs/dev/b,459
  > root/fe/tabulate,1865
  > root/ocaml,433
  > root/doc,3509
  > EOF
  |--------------------------|
  | feature      | megabytes |
  |--------------+-----------|
  | other-root   |       517 |
  |   child1     |       512 |
  |   shared     |      1803 |
  |     child    |       512 |
  | root         |      5244 |
  |   doc        |      3509 |
  |   emacs      |       455 |
  |     dev      |      1145 |
  |       a      |       460 |
  |       b      |       459 |
  |   fe         |           |
  |     tabulate |      1865 |
  |   ocaml      |       433 |
  |   patdiff    |      1046 |
  |     stuff    |      1003 |
  |--------------------------|

  $ echo '' | fe tools feature-table-of-csv

  $ echo -e '\n\n\n' | fe tools feature-table-of-csv
  ("invalid row(1): first column must be valid feature path" (""))
  [1]

  $ echo -e ',foo\nbar,baz\n,qux\n' | fe tools feature-table-of-csv
  ("invalid row(2): first column must be valid feature path" ("" qux))
  [1]

  $ echo 'feature' | fe tools feature-table-of-csv

  $ fe tools feature-table-of-csv <<EOF
  > column1
  > root
  > EOF
  |---------|
  | column1 |
  |---------|
  | root    |
  |---------|

  $ fe tools feature-table-of-csv -sep ';' <<EOF
  > feature;1;2
  > root;1;a
  > root/a;1;a;foo;bar
  > root;1
  > EOF
  |-----------------|
  | feature | 1 | 2 |
  |---------+---+---|
  | root    | 1 | a |
  | root    | 1 |   |
  |   a     | 1 | a |
  |-----------------|
