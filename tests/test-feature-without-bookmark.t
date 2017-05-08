  $ start_test
  $ setup_repo_and_root file

Initially, treated as up-to-date with a bookmark until the first hydra
synchronize state or worker query is received.

  $ fe show -has-bookmark
  true

Synchronize state removes the bookmark if it doesn't mention it.

  $ fe internal rpc-to-server call synchronize-state <<EOF
  > ((remote_repo_path $PWD)
  >  (bookmarks ()))
  > EOF
  ((bookmarks_to_rerun ()))
  $ fe show -has-bookmark
  false

Hydra worker adds the bookmark.

  $ feature_to_server root -fake-valid
  $ fe show -has-bookmark
  true

Persistence.

  $ fe-server stop
  $ fe-server start
  $ fe show -has-bookmark
  true

Restore bookmark is shown in next steps and the owner is alerted in his todo.

  $ fe internal rpc-to-server call synchronize-state <<EOF
  > ((remote_repo_path $PWD)
  >  (bookmarks ()))
  > EOF
  ((bookmarks_to_rerun ()))
  $ fe show -has-bookmark
  false
  $ fe show
  root
  ====
  root
  
  |-------------------------------------------------|
  | attribute              | value                  |
  |------------------------+------------------------|
  | next step              | restore bookmark       |
  | owner                  | unix-login-for-testing |
  | whole-feature reviewer | unix-login-for-testing |
  | seconder               | not seconded           |
  | review is enabled      | false                  |
  | CRs are enabled        | true                   |
  | reviewing              | unix-login-for-testing |
  | is permanent           | true                   |
  | has bookmark           | false                  |
  | tip                    | dc568be383d7           |
  | base                   | dc568be383d7           |
  |-------------------------------------------------|
  $ fe todo
  |----------------------------|
  | feature | next step        |
  |---------+------------------|
  | root    | restore bookmark |
  |----------------------------|
  
  Features you own:
  |----------------------------|
  | feature | next step        |
  |---------+------------------|
  | root    | restore bookmark |
  |----------------------------|
  $ fe tools force-retry |& matches "feature's bookmark is missing"
  [1]

Synchronize state adds the bookmark if it does mention it.

  $ tip=$(fe show -tip)
  $ fe internal rpc-to-server call synchronize-state <<EOF
  > ((remote_repo_path $PWD)
  >  (bookmarks (((bookmark root)
  >               (rev_info
  >                 ((first_12_of_rev ${tip:0:12})
  >                  (rev_author_or_error (Ok committer))))
  >               (status Done)
  >               (continuous_release_status Not_working_on_it)
  >               (compilation_status ())))))
  > EOF
  ((bookmarks_to_rerun ()))
  $ fe show -has-bookmark
  true

[fe tools restore-bookmark].

  $ hg book --delete root
  $ hg book
  no bookmarks set
  $ fe tools restore-bookmark root
  (error (prepare-to-restore-bookmark ("feature has a bookmark" root)))
  [1]
  $ fe internal rpc-to-server call synchronize-state <<EOF
  > ((remote_repo_path $PWD)
  >  (bookmarks ()))
  > EOF
  ((bookmarks_to_rerun ()))
  $ fe tools restore-bookmark root
  $ hg log -r root >/dev/null

Restore-bookmark did not update to the bookmark.

  $ hg active-bookmark
  [1]

And did not modify the known state in the server.

  $ fe show -has-bookmark root
  false
