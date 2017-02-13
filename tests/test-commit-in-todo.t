  $ start_test

Setup repo.

  $ setup_repo_and_root .fe.sexp file1 file2 file3
  $ mkdir .fe
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Define_scrutiny normal  ((level 50) (description "normal")))
  > (Define_scrutiny ignore  ((level  0) (description "ignore")))
  > (Users unix-login-for-testing user1 user2 user3)
  > EOF
  $ touch .fe/obligations-repo.sexp
  $ cat >.fe/.fe.sexp <<EOF
  > (Owner unix-login-for-testing)
  > (Scrutiny ignore)
  > (Apply_to All_files)
  > EOF
  $ cat >.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > (Reviewed_by (At_least 2 of_ (Users user1 user2 user3)))
  > (Apply_to (Files file1 file2 file3))
  > EOF
  $ hg add -q .
  $ hg com -m 'added files'
  $ fe enable
  $ fe change -set-base root
  $ feature_to_server root
  $ fe ob check
  $ fe second -even-though-owner -even-though-empty

Create child with changes.

  $ fe create root/child -d child
  $ for i in $(seq 1 3) ; do echo change >file${i}; done
  $ hg -q com -m file
  $ feature_to_server root/child
  $ fe enable-review root/child
  $ fe tools mark-fully-reviewed root/child
  $ fe second -even-though-owner
  $ fe show -omit-attribute-table -omit-description
  root/child
  ==========
  
  |---------------------------------------------|
  | user                   | review | completed |
  |------------------------+--------+-----------|
  | user1                  |      6 |           |
  | user2                  |      6 |           |
  | user3                  |      6 |           |
  | unix-login-for-testing |        |         6 |
  |---------------------------------------------|

  $ IRON_USER=user1 fe session mark-file root/child file1
  $ IRON_USER=user1 fe session mark-file root/child file2

  $ IRON_USER=user2 fe session mark-file root/child file2
  $ IRON_USER=user2 fe session mark-file root/child file3

  $ IRON_USER=user3 fe session mark-file root/child file3
  $ IRON_USER=user3 fe session mark-file root/child file1

  $ fe show -omit-attribute-table -omit-description
  root/child
  ==========
  
  |-----------------------------------------------------|
  | user                   |         review | completed |
  |------------------------+----------------+-----------|
  | user1                  | commit session |         4 |
  | user2                  | commit session |         4 |
  | user3                  | commit session |         4 |
  | unix-login-for-testing |                |         6 |
  |-----------------------------------------------------|
  
  |-----------------------------|
  | review sessions in progress |
  |-----------------------------|
  | user1                       |
  | user2                       |
  | user3                       |
  |-----------------------------|

  $ fe show -next-steps
  (Review)

  $ fe todo -for user1
  |--------------------------|
  | feature |         review |
  |---------+----------------|
  | root    |                |
  |   child | commit session |
  |--------------------------|

  $ IRON_USER=user1 fe session show
  Reviewing root/child to 5cad0a51f8cb.
  1 files to review (2 already reviewed): 6 lines total
  
  Required review.
     [X] 2 file1
     [X] 2 file2
  
  Optional review.
  The review obligations of these changes have been satisfied at this point.
  However, you may review them anyway if you desire.
     [ ] 2 file3

  $ fe todo -for user2
  |--------------------------|
  | feature |         review |
  |---------+----------------|
  | root    |                |
  |   child | commit session |
  |--------------------------|

  $ IRON_USER=user2 fe session show
  Reviewing root/child to 5cad0a51f8cb.
  1 files to review (2 already reviewed): 6 lines total
  
  Required review.
     [X] 2 file2
     [X] 2 file3
  
  Optional review.
  The review obligations of these changes have been satisfied at this point.
  However, you may review them anyway if you desire.
     [ ] 2 file1

  $ fe todo -for user3
  |--------------------------|
  | feature |         review |
  |---------+----------------|
  | root    |                |
  |   child | commit session |
  |--------------------------|

  $ IRON_USER=user3 fe session show
  Reviewing root/child to 5cad0a51f8cb.
  1 files to review (2 already reviewed): 6 lines total
  
  Required review.
     [X] 2 file1
     [X] 2 file3
  
  Optional review.
  The review obligations of these changes have been satisfied at this point.
  However, you may review them anyway if you desire.
     [ ] 2 file2
