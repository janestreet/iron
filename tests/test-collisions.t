  $ start_test

Create a couple of unrelated repos

  $ for i in {1..2}; do
  >   hg init repo$i
  >   echo a > repo$i/a
  >   hg --cwd repo$i addremove -q
  >   hg --cwd repo$i commit -m 'first commit' -u user$i
  >   eval rev0_$i=$(hg --cwd repo$i log -r . --template {node})
  > done
  $ [ $rev0_1 = $rev0_2 ]
  [1]

And now make sure that if bookmarks collide, the worker don't feed information
about one bookmark to the other guy:

  $ (cd repo1; fe create afeature -remote-repo-path "$PWD" -descr adescription)
  $ [ $(fe show afeature -tip) = $rev0_1 ]
  $ (cd repo2; hg book afeature -r .; feature_to_server afeature > /dev/null)
  $ [ $(fe show afeature -tip) = $rev0_1 ]
