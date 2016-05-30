  $ start_test
  $ setup_repo_and_root a b/c
  $ echo change >a; echo change >b/c; hg com -m change
  $ feature_to_server root -fake-valid
  $ fe diff -summary -file a
  |--------------------|
  | file | op  | lines |
  |------+-----+-------|
  | a    | mod |     2 |
  |--------------------|
  $ fe diff -summary -file ./a
  |--------------------|
  | file | op  | lines |
  |------+-----+-------|
  | a    | mod |     2 |
  |--------------------|
  $ fe diff -summary -file b/c
  |--------------------|
  | file | op  | lines |
  |------+-----+-------|
  | b/c  | mod |     2 |
  |--------------------|
  $ fe diff -summary -file ./b/c
  |--------------------|
  | file | op  | lines |
  |------+-----+-------|
  | b/c  | mod |     2 |
  |--------------------|
  $ fe diff -summary -file ./b/../a
  |--------------------|
  | file | op  | lines |
  |------+-----+-------|
  | a    | mod |     2 |
  |--------------------|
  $ fe diff -summary -file ./b/../b/c
  |--------------------|
  | file | op  | lines |
  |------+-----+-------|
  | b/c  | mod |     2 |
  |--------------------|
  $ cd b
  $ fe diff -summary -file c
  |--------------------|
  | file | op  | lines |
  |------+-----+-------|
  | b/c  | mod |     2 |
  |--------------------|
  $ fe diff -summary -file ../a
  |--------------------|
  | file | op  | lines |
  |------+-----+-------|
  | a    | mod |     2 |
  |--------------------|
  $ fe diff -summary -file ../b/c
  |--------------------|
  | file | op  | lines |
  |------+-----+-------|
  | b/c  | mod |     2 |
  |--------------------|
