  $ start_test
  $ setup_repo_without_root

Alphabetical.

  $ for f in {a,b,c,d}; do
  >   fe create $f -description 'root' -remote-repo-path $(pwd)
  > done

  $ fe list
  |------------------------------------|
  | feature |   lines | next step      |
  |---------+---------+----------------|
  | a       | pending | wait for hydra |
  | b       | pending | wait for hydra |
  | c       | pending | wait for hydra |
  | d       | pending | wait for hydra |
  |------------------------------------|

  $ echo '()' | fe internal rpc-to-server call "list-root-features" \
  >    | sexp query 'each (field root_feature)'
  a
  b
  c
  d

-depth

  $ fe create root -description 'root' -remote-repo-path $(pwd)
  $ for f in root/{a,b,c} root/b/{x,y}; do
  >     fe create $f -description $f
  > done
  $ fe list root
  |------------------------------------|
  | feature |   lines | next step      |
  |---------+---------+----------------|
  | root    | pending | wait for hydra |
  |   a     | pending | wait for hydra |
  |   b     | pending | wait for hydra |
  |   c     | pending | wait for hydra |
  |------------------------------------|
  $ for depth in 0 1 2 max; do
  >      echo "depth = $depth"
  >      echo '--------------'
  >      fe list -depth $depth
  >      echo
  > done
  depth = 0
  --------------
  
  depth = 1
  --------------
  |------------------------------------|
  | feature |   lines | next step      |
  |---------+---------+----------------|
  | a       | pending | wait for hydra |
  | b       | pending | wait for hydra |
  | c       | pending | wait for hydra |
  | d       | pending | wait for hydra |
  | root    | pending | wait for hydra |
  |------------------------------------|
  
  depth = 2
  --------------
  |------------------------------------|
  | feature |   lines | next step      |
  |---------+---------+----------------|
  | a       | pending | wait for hydra |
  | b       | pending | wait for hydra |
  | c       | pending | wait for hydra |
  | d       | pending | wait for hydra |
  | root    | pending | wait for hydra |
  |   a     | pending | wait for hydra |
  |   b     | pending | wait for hydra |
  |   c     | pending | wait for hydra |
  |------------------------------------|
  
  depth = max
  --------------
  |------------------------------------|
  | feature |   lines | next step      |
  |---------+---------+----------------|
  | a       | pending | wait for hydra |
  | b       | pending | wait for hydra |
  | c       | pending | wait for hydra |
  | d       | pending | wait for hydra |
  | root    | pending | wait for hydra |
  |   a     | pending | wait for hydra |
  |   b     | pending | wait for hydra |
  |     x   | pending | wait for hydra |
  |     y   | pending | wait for hydra |
  |   c     | pending | wait for hydra |
  |------------------------------------|
  

-name-only

  $ fe list -name-only
  a
  b
  c
  d
  root
  $ fe list -name-only root
  root
  root/a
  root/b
  root/c
  $ for depth in 0 1 2 max; do
  >      echo "depth = $depth"
  >      echo '--------------'
  >      fe list -name-only -depth $depth
  >      echo
  > done
  depth = 0
  --------------
  
  depth = 1
  --------------
  a
  b
  c
  d
  root
  
  depth = 2
  --------------
  a
  b
  c
  d
  root
  root/a
  root/b
  root/c
  
  depth = max
  --------------
  a
  b
  c
  d
  root
  root/a
  root/b
  root/b/x
  root/b/y
  root/c
  
List archived features.  The id and the archived time is give to make it easy to
find the id to give to [fe unarchive].

  $ fe archive root/a -reason "Reason for archiving that feature."
  $ fe archive root/c

  $ COLUMNS=500 fe list -archived root | grep -v -- ------ | single_space
  | feature | feature id | archived at | reason for archiving |
  | root | | | |
  | a | * | * | Reason for archiving that feature. | (glob)
  | c | * | * | | (glob)

  $ fe list -archived root -name-only -sort-most-recently-archived-first
  root/c
  root/a
