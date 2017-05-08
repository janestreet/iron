  $ start_test
  $ setup_repo_and_root file
  $ echo z >file2; hg add file2; echo change >file; hg com -m 'changes'
  $ feature_to_server root -fake-valid
  $ fe enable-review -add-whole-feature-reviewers user1
  $ fe change -set-reviewing-whole-feature-only

Brain starts empty.

  $ fe brain show

Clear an already empty brain.

  $ fe brain forget -all

Clearing an already empty brain interactively doesn't prompt.

  $ fe brain forget -all -interactive true
  Your brain is already empty; there is nothing to forget.

Mark, and the brain knows stuff.

  $ fe tools mark-fully-reviewed root -for unix-login-for-testing -reason reason
  $ fe brain show
  |---------------------|
  | file  | op  | lines |
  |-------+-----+-------|
  | file  | mod |     2 |
  | file2 | add |     1 |
  |---------------------|
  $ fe brain diff | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny level10
  base dc568be383d7 | tip a08ed6f3a513
  _
  | @@@@@@@@ Hunk 1/3 @@@@@@@@
  | @@@@@@@@ base 1,2 tip 1,2 @@@@@@@@
  | -|file
  | +|change
  |_
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file2 @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  base file = <absent>
  tip file  = file2
  _
  | @@@@@@@@ Hunk 2/3 @@@@@@@@
  | @@@@@@@@ base 1,2 tip 1,5 @@@@@@@@
  | -|<absent>
  | +|file        = file2
  | +|scrutiny    = level10
  | +|owner       = file-owner
  | +|reviewed by = None
  |_
  _
  | @@@@@@@@ Hunk 3/3 @@@@@@@@
  | @@@@@@@@ base 1,1 tip 1,2 @@@@@@@@
  | +|z
  |_
  $ fe brain diff -file file | fe internal remove-color
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ file @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  scrutiny level10
  base dc568be383d7 | tip a08ed6f3a513
  @@@@@@@@ base 1,2 tip 1,2 @@@@@@@@
  -|file
  +|change

Default when prompting is to not forget.

  $ echo | fe brain forget -all -interactive true
  |---------------------|
  | file  | op  | lines |
  |-------+-----+-------|
  | file  | mod |     2 |
  | file2 | add |     1 |
  |---------------------|
  
  Really forget above diffs for [root]? [y/N]: Quit

Forget, answer "y" to the prompt, and the brain is empty.

  $ echo y | fe brain forget -all -interactive true
  |---------------------|
  | file  | op  | lines |
  |-------+-----+-------|
  | file  | mod |     2 |
  | file2 | add |     1 |
  |---------------------|
  
  Really forget above diffs for [root]? [y/N]:  (no-eol)
  $ fe brain show

Mark and then clear each file separately.

  $ fe tools mark-fully-reviewed root -for unix-login-for-testing
  $ fe brain show
  |---------------------|
  | file  | op  | lines |
  |-------+-----+-------|
  | file  | mod |     2 |
  | file2 | add |     1 |
  |---------------------|
  $ fe brain forget -file file
  $ fe brain show
  |---------------------|
  | file  | op  | lines |
  |-------+-----+-------|
  | file2 | add |     1 |
  |---------------------|
  $ fe brain forget -file file2
  $ fe brain show

Check that forget is cleaning catch-up if any.

  $ fe todo -for user1
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |      4 |
  |------------------|
  $ fe tools mark-fully-reviewed root -for user1 -reason testing
  $ fe todo -for user1
  |--------------------|
  | feature | catch-up |
  |---------+----------|
  | root    |        4 |
  |--------------------|
  $ IRON_USER=user1 fe brain forget -file file
  $ fe todo -for user1
  |-----------------------------|
  | feature | review | catch-up |
  |---------+--------+----------|
  | root    |      2 |        2 |
  |-----------------------------|
  $ IRON_USER=user1 fe brain forget -file file2
  $ fe todo -for user1
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |      4 |
  |------------------|
  $ fe tools mark-fully-reviewed root -for user1 -reason testing
  $ fe todo -for user1
  |--------------------|
  | feature | catch-up |
  |---------+----------|
  | root    |        4 |
  |--------------------|

When [fe brain forget] is run by another user, this preserves any catch-up.

  $ fe brain forget -all -for user1
  $ fe todo -for user1
  |-----------------------------|
  | feature | review | catch-up |
  |---------+--------+----------|
  | root    |      4 |        4 |
  |-----------------------------|

  $ IRON_USER=user1 fe catch-up clear
  $ fe todo -for user1
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |      4 |
  |------------------|
  $ fe tools mark-fully-reviewed root -for user1 -reason testing
  $ fe todo -for user1
  |--------------------|
  | feature | catch-up |
  |---------+----------|
  | root    |        4 |
  |--------------------|

But when [fe brain forget] is run by the actual user, the catch-up is cleared.

  $ IRON_USER=user1 fe brain forget -all
  $ fe todo -for user1
  |------------------|
  | feature | review |
  |---------+--------|
  | root    |      4 |
  |------------------|

Mark and then forget both files.

  $ fe tools mark-fully-reviewed root -for unix-login-for-testing
  $ fe brain show
  |---------------------|
  | file  | op  | lines |
  |-------+-----+-------|
  | file  | mod |     2 |
  | file2 | add |     1 |
  |---------------------|
  $ fe brain forget -file file -file file2
  $ fe brain show

Can't forget a file you don't know.

  $ fe brain forget -file file |& matches "there is no diff in your brain for"
  [1]

Create a session with no review, and one can still clear the brain, even if the
session is locked as long as it has no reviewed files.

  $ fe session show
  Reviewing root to a08ed6f3a513.
  2 files to review: 4 lines total
     [ ] 2 file
     [ ] 2 file2
  $ fe session lock
  $ fe brain forget -all

But if review has been done in the session, one cannot clear the brain.

  $ fe session mark-file root file
  $ fe session show
  Reviewing root to a08ed6f3a513.
  1 files to review (1 already reviewed): 4 lines total
     [X] 2 file
     [ ] 2 file2
  $ fe brain forget -all |& matches "cannot forget"
  [1]
