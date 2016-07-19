  $ start_test
  $ setup_repo_and_root file
  $ echo z >file2; hg add file2
  $ for i in {1..13}; do echo change >>file; done
  $ hg com -m 'changes'
  $ feature_to_server root -fake-valid
  $ fe enable

Review [file], and the brain knows the right number of lines.

  $ fe session mark-file root file -reason reason
  $ fe session commit -session-id $(fe session show -id)
  $ fe brain show
  |--------------------|
  | file | op  | lines |
  |------+-----+-------|
  | file | mod |    13 |
  |--------------------|

Ditto, with server restart during the session.

  $ fe brain forget -file file
  $ fe brain show
  $ fe session mark-file root file -reason reason
  $ fe-server stop
  $ fe-server start
  $ fe session commit -session-id $(fe session show -id)
  $ fe brain show
  |--------------------|
  | file | op  | lines |
  |------+-----+-------|
  | file | mod |    13 |
  |--------------------|
