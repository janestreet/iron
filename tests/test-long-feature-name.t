Start test.

  $ start_test

Create hg repo.

  $ mkdir repo
  $ cd repo
  $ hg init
  $ touch file
  $ hg add file
  $ hg com -m file

Create root.

  $ fe create root -description 'root' -remote-repo-path $(pwd)

Create chain.

  $ parent='root'
  $ for i in {1..10}; do
  >     child=$parent/child-at-depth-$i
  >     fe create $child -description "Child at depth $i."
  >     parent=$child
  > done

Show it.

  $ fe show $parent -omit-attribute-table
  root/child-at-depth-1/child-at-depth-2/child-at-depth-3/child-at-depth-4/child-at-depth-5/child-at-depth-6/child-at-depth-7/child-at-depth-8/child-at-depth-9/child-at-depth-10
  
  root
    child-at-depth-1
      child-at-depth-2
        child-at-depth-3
          child-at-depth-4
            child-at-depth-5
              child-at-depth-6
                child-at-depth-7
                  child-at-depth-8
                    child-at-depth-9
                      child-at-depth-10
  ==========================================================================================
  Child at depth 10.
