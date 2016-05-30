  $ start_test

Test -- archive a feature whose bookmark is not in the local clone

  $ mkdir remote
  $ cd remote
  $ hg init
  $ echo c > f
  $ hg add f
  $ hg ci -m '0'
  $ cd ../
  $ hg clone remote local -q

Create the feature in remote:

  $ cd remote
  $ fe create root -desc root -remote $(pwd) 

Archive it in local *without pulling the bookmark*:

  $ cd ../local
  $ hg bookmarks | matches 'no bookmarks'
  $ fe archive root
  $ hg --cwd ../remote bookmarks | matches 'no bookmarks'
