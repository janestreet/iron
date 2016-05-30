Start test.

  $ start_test

  $ setup_repo_and_root file
  $ fe change -set-owner owner
  $ cat >file <<EOF
  > # $CR-soon user1 for user2:
  > EOF
  $ hg com -m 'added CR-soon'
  $ feature_to_server root -fake-valid-obligations

Check output of todo

  $ fe todo -for user2
  CR-soons assigned to you:
  |--------------------|
  | family | file:line |
  |--------+-----------|
  | root   | file:1    |
  |--------------------|

Change the .ferc file

  $ export HOME=$IRON_APPDIR/home
  $ mkdir -p $HOME
  $ cat > $HOME/.ferc <<EOF
  > (add_flag_to "todo" -do-not-show-cr-soons)
  > EOF

Check output of todo

  $ fe todo -for user2

Change the text to not be a CR-soon.

  $ cat >file <<EOF
  > A change.
  > EOF
  $ hg com -m change
  $ feature_to_server root -fake-valid-obligations

Create a child feature.

  $ fe create root/child -desc desc -base $(fe show root -base)
  $ cat >file <<EOF
  > A different change
  > EOF
  $ hg com -q -m change
  $ feature_to_server root/child -fake-valid-obligations

Setup .ferc to use a custom merge tool.

  $ merge=$HOME/merge
  $ touch $merge
  $ chmod a+x $merge
  $ cat >$HOME/.ferc <<EOF
  > (add_flag_to "rebase" -merge-tool
  >   ((executable "$merge")
  >    (args "\$local \$base \$other")))
  > EOF
  $ fe tools validate-ferc

  $ cat >$HOME/broken-ferc <<EOF
  > (this_is_broken)
  > EOF
  $ fe tools validate-ferc $HOME/broken-ferc |& matches "(this_is_broken)"
  [1]
  $ rm -f  $HOME/broken-ferc
  $ fe tools validate-ferc $HOME/broken-ferc |& matches "file not found.*broken-ferc"
  [1]
  $ cp $HOME/.ferc $HOME/working-ferc
  $ fe tools validate-ferc $HOME/working-ferc

Rebase fails when custom merge tool exits nonzero.

  $ cat >$merge <<'EOF'
  > #!/bin/bash
  > exit 1
  > EOF
  $ fe rebase |& matches "aborted rebase because merge failed"
  [1]
  $ hg status
  $ hg active-bookmark
  root/child
  $ cat file
  A different change
  $ feature_to_server root/child -fake-valid-obligations
  $ [ $(fe show -base) = $(fe show root -tip) ]
  [1]
  $ fe show -next-step
  (Rebase Enable_review)

Rebase succeeds when custom merge tool exits zero.

  $ cat >$merge <<'EOF'
  > #!/bin/bash
  > ( echo "File 1"; cat $1; echo -e "\nFile 2"; cat $2; echo -e "\nFile 3"; cat $3 ) >tmp
  > mv tmp $1
  > EOF
  $ fe rebase
  merging file
  0 files updated, 1 files merged, 0 files removed, 0 files unresolved
  (branch merge, don't forget to commit)
  $ hg active-bookmark
  root/child
  $ cat file
  File 1
  A different change
  
  File 2
  file
  
  File 3
  A change.
  $ hg status
  $ feature_to_server root/child -fake-valid-obligations
  $ [ $(fe show -base) = $(fe show root -tip) ]
  $ fe show -next-step
  (Enable_review)

