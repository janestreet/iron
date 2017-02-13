  $ start_test
  $ setup_repo_and_root file
  $ echo change >file; hg com -m change
  $ feature_to_server root -fake-valid
  $ fe enable

Mark the user.

  $ fe tools mark-fully-reviewed root -for unix-login-for-testing

Add new commits, leaving the feature unchanged.

  $ echo change2 >file; hg com -m change2
  $ feature_to_server root -fake-valid
  $ echo change >file; hg com -m change
  $ feature_to_server root -fake-valid

Make sure the diff4s needed are at the review goal.

  $ fe show -base
  dc568be383d74aa2aadbbcea8df3869d792d7ff4
  $ fe show -tip
  a97190e4962cd43a5659f998974ba2b36d713c9d
  $ fe internal need-diff4s-starting-from
  ((((base
      ((human_readable ())
       (node_hash dc568be383d74aa2aadbbcea8df3869d792d7ff4)))
     (tip
      ((human_readable ())
       (node_hash a97190e4962cd43a5659f998974ba2b36d713c9d))))
    (unix-login-for-testing)))

Start a session.

  $ touch file2; hg add file2; hg com -m file2
  $ echo change2 >file; hg com -m change2
  $ feature_to_server root -fake-valid
  $ fe session mark-file root file
  $ fe session show
  Reviewing root to 7c2ded74c530.
  1 files to review (1 already reviewed): 3 lines total
     [X] 2 file
     [ ] 1 file2

Extend the feature with a no-op.

  $ echo change  >file; hg com -m change
  $ echo change2 >file; hg com -m change2
  $ feature_to_server root -fake-valid

Finish the session.

  $ fe session mark-file root file2

The reviewer brain has been advanced to the goal.

  $ fe show -tip
  0ede794dd7f0cae975cd43b55acd6bbaf225bf08
  $ fe internal need-diff4s-starting-from
  ((((base
      ((human_readable ())
       (node_hash dc568be383d74aa2aadbbcea8df3869d792d7ff4)))
     (tip
      ((human_readable ())
       (node_hash 0ede794dd7f0cae975cd43b55acd6bbaf225bf08))))
    (file-owner unix-login-for-testing)))
