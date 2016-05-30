Start test. 

  $ start_test

  $ setup_repo_and_root file
  $ hg active-bookmark
  root
  $ fe create root/child -desc child
  $ hg active-bookmark
  root/child
  $ fe up root
  $ hg active-bookmark
  root
  $ fe up root/child
  $ hg active-bookmark
  root/child
 
