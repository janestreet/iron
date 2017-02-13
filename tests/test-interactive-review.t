Check expected behavior around the interactive [fe review] command.


  $ function review {
  >   fe review -interactive true "$@"
  > }

  $ export NUM=10

  $ start_test
  $ setup_repo_and_root $(for i in $(seq 1 ${NUM}) ; do echo "f${i}" ; done)

  $ BASE=$(fe show -base)

  $ for i in $(seq 1 ${NUM}) ; do
  >   echo change >"f${i}"
  > done
  > hg commit -m base
  > feature_to_server root -fake-valid

  $ fe change -add-whole-feature-reviewers user1
  $ fe change -set-reviewing whole-feature-reviewers
  $ fe enable-review

  $ fe show -omit-attribute-table
  root
  ====
  root
  
  |---------------------------------|
  | user                   | review |
  |------------------------+--------|
  | unix-login-for-testing |     20 |
  | user1                  |     20 |
  |---------------------------------|

Quit the review without having reviewed anything.

  $ echo 'q' | review | matches "Quit"
  $ fe internal session show-num-lines
  20

Requesting help on the first menu.

  $ echo -e "?\nq" | review | grep -A 30 'How do you want to do this review?'
  How do you want to do this review? [F/g/s/c/q/?]: F : Enter file-by-file mode
  g : See all the remaining hunks at once
  s : Select a subset of the files
  c : Commit the current session and review the remainder to the current tip
  q : Quit
  ? : Print this help
  How do you want to do this review? [F/g/s/c/q/?]: Quit

Requesting help on the file menu.

  $ echo -e "F\n?\nq" | review | grep -A 30 'Mark as reviewed?'
  [1/10] f1 Mark as reviewed? [A/y/n/p/e/c/g/s/q/?]: A : Show the current file again
  y : Mark the current file as reviewed
  n : Skip the current file for now. Go to the next one
  p : Skip the current file for now. Go back to the previous one
  e : Open current file in Emacs
  c : Commit the current session and review the remainder to the current tip
  g : See all the remaining hunks at once
  s : Select a subset of the files
  q : Quit
  ? : Print this help
  [1/10] f1 Mark as reviewed? [A/y/n/p/e/c/g/s/q/?]: Quit

Reviewing only specific files.

  $ echo -e "F\ny\ny" | review -file f1 -file f10 | matches "Current session reviewed."
  $ fe session show
  Reviewing root to *. (glob)
  8 files to review (2 already reviewed): 20 lines total
     [X] 2 f1
     [X] 2 f10
     [ ] 2 f2
     [ ] 2 f3
     [ ] 2 f4
     [ ] 2 f5
     [ ] 2 f6
     [ ] 2 f7
     [ ] 2 f8
     [ ] 2 f9

Review some file, and not some others.

  $ echo -e "F\nn\ny\nn\ny\nn\ny\n\nn\ny\nq" | review | matches "Quit"
  $ fe session show
  Reviewing root to *. (glob)
  4 files to review (6 already reviewed): 20 lines total
     [X] 2 f1
     [X] 2 f10
     [ ] 2 f2
     [X] 2 f3
     [ ] 2 f4
     [X] 2 f5
     [ ] 2 f6
     [X] 2 f7
     [ ] 2 f8
     [X] 2 f9

One can select only a subset of the files via the select mode.

  $ echo -e "s\nf6\n\ny\nq" | review | matches "Current selection to show:.* 2 f6.*Quit"
  $ fe session show
  Reviewing root to *. (glob)
  3 files to review (7 already reviewed): 20 lines total
     [X] 2 f1
     [X] 2 f10
     [ ] 2 f2
     [X] 2 f3
     [ ] 2 f4
     [X] 2 f5
     [X] 2 f6
     [X] 2 f7
     [ ] 2 f8
     [X] 2 f9

  $ fe session forget -file f1 -session-id $(fe session show -id)

One can review some files, then commit the session in the middle of review and
carry one with the next session.

  $ echo -e "F\ny\nc\ny\nF\nn\ny\nq" | review \
  >   | matches "Really commit this root session?.* f4 Mark as reviewed?.*Quit"

  $ fe session show
  Reviewing root to *. (glob)
  2 files to review (1 already reviewed): 6 lines total
     [ ] 2 f2
     [X] 2 f4
     [ ] 2 f8

One can use [fe review] to review on behalf of someone else.  When doing so,
supplying a reason is required.

  $ echo -e "F\ny\nq" | IRON_USER=user1 review -for unix-login-for-testing 2>&1 \
  >   | matches "must supply -reason when acting for someone"
  [1]

  $ echo -e "F\ny\nq" \
  >   | IRON_USER=user1 review -for unix-login-for-testing -reason 'reason' >/dev/null

When there are catch-up lines to review, they are shown first ...

  $ echo -e "q\n" | review | grep -A 300 'Catch-up.'
  Catch-up.  user1 reviewed this for you, giving the reason as:
  reason
     [ ] 2 f2
  How do you want to do this review? [F/g/s/q/?]: Quit

... unless one supplies [-skip-catch-up-review] ...

  $ echo -e "q\n" | review -skip-catch-up-review | grep -A 300 'Reviewing.'
  Reviewing root to *. (glob)
  1 files to review (2 already reviewed): 6 lines total
     [X] 2 f2
     [X] 2 f4
     [ ] 2 f8
  How do you want to do this review? [F/g/s/c/q/?]: Quit

... or unless someone else is reviewing on their behalf.

  $ echo -e "q\n" | IRON_USER=user1 review -for unix-login-for-testing -reason test \
  >   | grep -A 300 'Reviewing.'
  Reviewing root to *. (glob)
  1 files to review (2 already reviewed): 6 lines total
     [X] 2 f2
     [X] 2 f4
     [ ] 2 f8
  How do you want to do this review? [F/g/s/c/q/?]: Quit

Check that some reasonable errors are given when inconsistent combinations of
the switches are supplied.

  $ IRON_USER=user1 review -create-catch-up-for-me -for unix-login-for-testing
  Cannot use [-create-catch-up-for-me] when reviewing for other users only.
  [1]

  $ review -only-catch-up-review -for unix-login-for-testing -create-catch-up-for-me
  At most one of the switches -only-catch-up-review and -create-catch-up-for-me may be supplied.
  [1]

  $ review -only-catch-up-review -skip-catch-up-review
  At most one of the switches -only-catch-up-review and -skip-catch-up-review may be supplied.
  [1]

Check that an admin can catch-up for someone else (in fact in test, this is
always allowed).

  $ fe catch-up show -omit-attribute-table -omit-header-and-description
  Reviewing root to * (glob)
  1 files to review: 2 lines total
  
  Catch-up.  user1 reviewed this for you, giving the reason as:
  reason
     [ ] 2 f2

  $ echo -e 'F\ny\nq\n' \
  >   | IRON_USER=user1 review -only-catch-up-review -for unix-login-for-testing >/dev/null

  $ fe catch-up show
  No review to catch up on for unix-login-for-testing in root.
  [1]

When there is no catch-up, the command does not enter the regular review loop,
and the proper indication is printed on stdout.

  $ echo -e 'F\ny\nq\n' \
  >   | IRON_USER=user1 review -only-catch-up-review -for unix-login-for-testing
  No review to catch up on in root.
