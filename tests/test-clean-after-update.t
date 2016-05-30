  $ start_test

Checking that we keep the status clean when we update

  $ setup_repo_and_root a
  $ echo 'glob:*.ignored' > .hgignore
  $ hg add .hgignore
  $ hg commit -m 'update hgignore'
  $ touch a.ignored
  $ hg status
  $ fe create root/feature -base 0 -desc 'wah the error is terrible'
  $ hg status
