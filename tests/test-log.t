Start test.

  $ start_test

Create hg repo.

  $ setup_repo_and_root file
  $ touch file2; hg add file2; hg com -m 'file2' file2
  $ touch file3; hg add file3; hg com -m 'file3' file3
  $ feature_to_server root

The log is in reverse-chronological order.

  $ fe log root -- --template='{rev}\n'
  2
  1

The log is human readable.

  $ fe log root
  changeset:   2:5751b9463335
  bookmark:    root
  tag:         tip
  user:        test
  date:        * (glob)
  summary:     file3
  
  changeset:   1:d2f2e888d0d3
  user:        test
  date:        * (glob)
  summary:     file2
  
