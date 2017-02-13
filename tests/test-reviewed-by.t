Start test.

  $ start_test

Setup repo.

  $ setup_repo_and_root .fe.sexp
  $ mkdir .fe
  $ all_users=$(echo user{1..3})
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Define_scrutiny normal ((level 50) (description "normal")))
  > (Users unix-login-for-testing $all_users)
  > (Define_group users123 ($all_users))
  > EOF
  $ touch .fe/obligations-repo.sexp
  $ cat >.fe/.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > EOF
  $ touch .
  $ hg add -q .
  $ hg com -m 'added files'
  $ fe enable

Helper functions for setting review-obligations of [file].

  $ function set_reviewed_by {
  >     if [ -n "${2:-}" ]; then
  >       followers="(Followers $2)"
  >     else
  >       followers=""
  >     fi
  >     cat >.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Reviewed_by $1)
  > $followers
  > (Apply_to All_files)
  > EOF
  >     hg com -m 'updated reviewed-by'
  >     feature_to_server root
  >     fe ob check
  > }

Get valid obligations.

  $ set_reviewed_by '(All_of (Users unix-login-for-testing))'
  $ fe change -set-base root
  $ feature_to_server root
  $ fe ob check
  $ fe second -even-though-owner -even-though-empty

Multiple missing .fe.sexp files.

  $ rm .fe.sexp .fe/.fe.sexp
  $ fe ob check
  ("deleted but tracked .fe.sexp files" (.fe/.fe.sexp .fe.sexp))
  [1]
  $ hg revert -q .

Helper function for checking whether various sets of reviewers satisfy a reviewed-by.

  $ function check {
  >     set_reviewed_by "$1" ""
  >     for users in '' user{1..3} "$(echo user{1,2})" "$(echo user{1..3})"; do
  >         for user in $all_users; do
  >             fe brain forget -all -for $user >/dev/null 2>&1 || true
  >         done
  >         fe tools mark-fully-reviewed root >/dev/null 2>&1
  >         for user in $users; do
  >             fe tools mark-fully-reviewed root -for $user -reason reason \
  >                 >/dev/null 2>&1 || true
  >         done
  >         is_releasable=$(if fe is-releasable >/dev/null 2>/dev/null; then echo yes; else echo no; fi)
  >         printf "%3s: %s\n" "$is_releasable" "$users"
  >     done
  > }

  $ function check_followers {
  >     if set_reviewed_by "(All_of (Users user1))" "$1"; then
  >       echo "$(fe obl show .fe.sexp | sexp select followers | sexp query each | xargs echo)"
  >     fi
  > }

Just checks a few cases.  More tests are implemented as OCaml unit tests in [reviewed_by.ml].

  $ check "(All_of (Users user1))"
   no: 
  yes: user1
   no: user2
   no: user3
  yes: user1 user2
  yes: user1 user2 user3

  $ check "(Or)"
  ("invalid .fe.sexp file"
   (.fe.sexp:3:13 "[Or] must have at least one clause"))
   no: 
   no: user1
   no: user2
   no: user3
   no: user1 user2
   no: user1 user2 user3

  $ check "(At_least 2 of_ (Users user1 user2 user3))"
   no: 
   no: user1
   no: user2
   no: user3
  yes: user1 user2
  yes: user1 user2 user3

  $ check_followers user1
  user1
  $ check_followers "user2 user1"
  user1 user2
  $ check_followers "(Users user2 user1)"
  user1 user2
  $ check_followers "(Group users123) unix-login-for-testing"
  unix-login-for-testing user1 user2 user3
  $ check_followers "(Group users123) (Users unix-login-for-testing)"
  unix-login-for-testing user1 user2 user3
  $ check_followers "(Group unknown-group)"
  ("invalid .fe.sexp file" (.fe.sexp:4:0 ("no such group" unknown-group)))
  $ check_followers "unknown-user"
  ("invalid .fe.sexp file"
   (.fe.sexp:4:0
    ("users not in obligations-global.sexp or obligations-repo.sexp"
     (unknown-user))))
