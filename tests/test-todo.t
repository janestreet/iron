Start test. 

  $ start_test

Create hg repo.  

  $ mkdir repo
  $ cd repo
  $ hg init
  $ touch file
  $ hg add file
  $ hg com -m file
  $ remote=$(pwd)
  $ fe create root -owner owner -description root -remote-repo-path $remote
  $ cat >file <<EOF
  > # $CR user1 for user2:
  > EOF
  $ hg com -m 'added CR'
  $ feature_to_server root -fake-valid-obligations
  $ fe enable-review
  $ fe show
  root
  ====
  root
  
  |---------------------------------------|
  | attribute              | value        |
  |------------------------+--------------|
  | next step              | CRs, review  |
  | owner                  | owner        |
  | whole-feature reviewer | owner        |
  | seconder               | not seconded |
  | review is enabled      | true         |
  | reviewing              | owner        |
  | is permanent           | false        |
  | tip                    | 493a6eeffeb0 |
  |   tip is cr clean      | false        |
  | base                   | 04da3968e088 |
  |---------------------------------------|
  
  |---------------------|
  | user  | CRs | total |
  |-------+-----+-------|
  | user2 |   1 |     1 |
  | total |   1 |     1 |
  |---------------------|
  
  |----------------|
  | user  | review |
  |-------+--------|
  | owner |      1 |
  |----------------|
  $ fe todo -for owner
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |      1 |
  |------------------|
  
  Features you own:
  |-------------------------------------|
  | feature | CRs | #left | next step   |
  |---------+-----+-------+-------------|
  | root    |   1 |     1 | CRs, review |
  |-------------------------------------|
  $ fe todo -crs-and-review -for owner
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |      1 |
  |------------------|
  $ fe todo -owned-by-me -for owner
  Features you own:
  |-------------------------------------|
  | feature | CRs | #left | next step   |
  |---------+-----+-------+-------------|
  | root    |   1 |     1 | CRs, review |
  |-------------------------------------|
  $ fe todo -crs-and-review -owned-by-me -for owner
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |      1 |
  |------------------|
  
  Features you own:
  |-------------------------------------|
  | feature | CRs | #left | next step   |
  |---------+-----+-------+-------------|
  | root    |   1 |     1 | CRs, review |
  |-------------------------------------|
  $ fe todo -crs-and-review-names -for owner
  root
  $ fe todo -crs-and-review-names -for user1
  $ fe todo -crs-and-review-names -for user2
  root
  $ fe todo -review-names -for owner
  root
  $ fe todo -review-names -for user1
  $ fe todo -review-names -for user2
  $ fe todo -owned-by-me-names -for owner
  root
  $ fe todo -owned-by-me-names -for user1
  $ fe todo -owned-by-me-names -for user2

  $ fe todo -for user2
  |---------------|
  | feature | CRs |
  |---------+-----|
  | root    |   1 |
  |---------------|
  $ fe disable-review -and-crs root
  $ fe show -review-is-enabled
  false
  $ fe show -crs-are-enabled
  false
  $ fe todo -for user2
  $ fe change -set-crs-are-enabled true
  $ fe todo -for user2
  |---------------|
  | feature | CRs |
  |---------+-----|
  | root    |   1 |
  |---------------|
  $ fe enable-review root
  $ fe show -crs-are-enabled
  true
  $ fe change -set-crs-shown-in-todo-only-for-users-reviewing true
  $ fe show | grep 'shown in todo'
  | CRs shown in todo for  | users reviewing only |
  $ fe todo -for user2
  $ fe change -add-reviewing user2
  $ fe todo -for user2
  |---------------|
  | feature | CRs |
  |---------+-----|
  | root    |   1 |
  |---------------|
  $ fe change -set-crs-shown-in-todo-only-for-users-reviewing false
  $ fe show | grep 'shown in todo'
  [1]
  $ fe change -remove-reviewing user2
  $ fe todo -for user2
  |---------------|
  | feature | CRs |
  |---------+-----|
  | root    |   1 |
  |---------------|

  $ cat >file <<EOF
  > # $XCR user1 for user2:
  > EOF
  $ hg com -m 'added XCR'
  $ feature_to_server root -fake-valid-obligations

  $ fe change -set-xcrs-shown-in-todo-only-for-users-reviewing true
  $ fe show | grep 'shown in todo'
  | XCRs shown in todo for | users reviewing only |
  $ fe todo -for user1
  $ fe change -add-reviewing user1
  $ fe todo -for user1
  |----------------|
  | feature | XCRs |
  |---------+------|
  | root    |    1 |
  |----------------|
  $ fe change -set-xcrs-shown-in-todo-only-for-users-reviewing false
  $ fe show | grep 'shown in todo'
  [1]
  $ fe change -remove-reviewing user1
  $ fe todo -for user1
  |----------------|
  | feature | XCRs |
  |---------+------|
  | root    |    1 |
  |----------------|

  $ cat >file <<EOF
  > # $CR user1 for user2:
  > EOF
  $ hg com -m 'added CR'
  $ feature_to_server root -fake-valid-obligations

  $ fe tools mark-fully-reviewed root -for owner -reason reason

  $ with_visible_colors fe todo -for owner
  |--------------------|
  | feature | catch-up |
  |---------|----------|
  | <yellow>root<off>    |        1 |
  |--------------------|
  
  Features you own:
  |---------------------------|
  | feature | CRs | next step |
  |---------|-----|-----------|
  | <yellow>root<off>    |   1 | <yellow>CRs<off>       |
  |---------------------------|
  $ fe todo -catch-up-names -for owner
  root

Archived features show up in dim in the todo.

  $ fe archive root -for owner
  $ with_visible_colors fe todo -for owner
  |--------------------|
  | feature | catch-up |
  |---------|----------|
  | <dim>root<off>    |        1 |
  |--------------------|
