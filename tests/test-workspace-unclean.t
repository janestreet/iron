  $ export IRON_FUNCTIONAL_TESTING_FORCE_WORKSPACES=true
  $ start_test
  $ setup_repo_and_root f1.ml

  $ list_unclean_workspaces () {
  >   case "$@" in
  >     -no-update)
  >       ;;
  >     '')
  >       fe workspace unclean update-server -all >/dev/null
  >       ;;
  >     *)
  >       echo "unknown flags: $@" > /dev/stderr
  >       exit 1
  >       ;;
  >   esac
  >   fe todo -unclean-workspaces | sed "s;$HOSTNAME;\$HOSTNAME;"
  > }

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

Check shelves.

  $ ROOT=$PWD
  $ cd $(fe workspace dir root/child2)

  $ hg shelve --list         --config extensions.shelve=
  $ echo 'foo' > foo

  $ hg shelve -A foo -n foo  --config extensions.shelve=
  shelved as foo
  0 files updated, 0 files merged, 1 files removed, 0 files unresolved

  $ cp $HOME/.ferc $HOME/.ferc.backup
  $ cat >>$HOME/.ferc <<EOF
  > (workspaces (
  >   (unclean_workspaces_detection_includes_shelved_changes false)
  > ))
  > EOF

  $ fe workspace unclean check root/child2
  $ list_unclean_workspaces

  $ mv $HOME/.ferc.backup $HOME/.ferc

  $ fe workspace unclean check root/child2
  (errors (((feature_path root/child2) (reason ("shelved changes")))))
  [1]

  $ list_unclean_workspaces
  Unclean workspaces on $HOSTNAME:
  |----------------------------|
  | feature  | reason          |
  |----------+-----------------|
  | root     |                 |
  |   child2 | shelved changes |
  |----------------------------|

  $ hg unshelve  --config extensions.shelve=
  unshelving change 'foo'
  $ rm foo
  $ hg rm -A foo

  $ cd ${ROOT}

  $ fe workspace unclean check root/child2
  $ list_unclean_workspaces

Detect uncommitted changes.

  $ touch $(fe workspace dir root/child2)/FILE
  $ list_unclean_workspaces
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

  $ grep -q -F 'hg_status"? FILE\n' $(fe workspace dir root/child2)/.hg/iron.*.log

Check that this is persisted.

  $ fe-server stop
  $ fe-server start

  $ list_unclean_workspaces -no-update
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

  $ list_unclean_workspaces -no-update
  $ fe show root/child2 -omit-attribute-table
  root/child2
  ===========
  child2

  $ fe remind root/child2 -just-print-recipients-and-exit
  owner

Detect unpushed changesets.

  $ cd $(fe workspace dir root/child2)
  $ echo c2 > f1.ml
  $ hg ci -m testing
  $ list_unclean_workspaces
  Unclean workspaces on $HOSTNAME:
  |--------------------------------|
  | feature  | reason              |
  |----------+---------------------|
  | root     |                     |
  |   child2 | unpushed changesets |
  |--------------------------------|
  $ hg push -q ; feature_to_server root/child2 -fake-valid-obligations
  $ list_unclean_workspaces

Detect invalid current bookmark.
  $ hg up -r .^ -q ; hg active
  [1]
  $ list_unclean_workspaces
  Unclean workspaces on $HOSTNAME:
  |-------------------------------------|
  | feature  | reason                   |
  |----------+--------------------------|
  | root     |                          |
  |   child2 | invalid current bookmark |
  |-------------------------------------|
  $ hg up -q -r root/child2
  $ list_unclean_workspaces
