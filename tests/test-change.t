Start test.

  $ start_test

Setup.

  $ mkdir repo
  $ cd repo
  $ hg init
  $ echo "this is a line" > a.txt
  $ hg add a.txt && hg commit -m "c1"
  $ base1=$(hg id | cut -d ' ' -f 1)
  $ echo "this is another line" >> a.txt
  $ hg commit -m "c2"
  $ base2=$(hg id | cut -d ' ' -f 1)

Can't change nonexistent feature.

  $ fe change nonexistent-feature -add-owners "jdoe1"
  ("no such feature" nonexistent-feature)
  [1]

Create feature

  $ fe create root -description 'root' -remote-repo-path $(pwd)

add owner

  $ fe show root -first-owner
  unix-login-for-testing

  $ fe change root -add-owners "jdoe1,jdoe4" -verbose
  ((Add_owners (jdoe1 jdoe4)) (Ok ()))

  $ fe show root -owners
  (unix-login-for-testing jdoe1 jdoe4)

  $ fe change root -add-owners "jdoe2,jdoe3" -verbose
  ((Add_owners (jdoe2 jdoe3)) (Ok ()))

  $ fe show root -owners
  (unix-login-for-testing jdoe1 jdoe4 jdoe2 jdoe3)

  $ fe show root -first-owner
  unix-login-for-testing

remove owner

  $ fe change root -remove-owners "jdoe1,jdoe3" -verbose
  ((Remove_owners (jdoe1 jdoe3)) (Ok ()))

  $ fe show root -owners
  (unix-login-for-testing jdoe4 jdoe2)

  $ fe change root -remove-owners "jdoe1"
  ("there were problems"
   (((Remove_owners (jdoe1)) (Error ("not currently an owner" (jdoe1))))))
  [1]

  $ fe change root -remove-owners "jdoe2,jdoe4,unix-login-for-testing"
  ("there were problems"
   (((Remove_owners (jdoe2 jdoe4 unix-login-for-testing))
     (Error "must have at least one owner"))))
  [1]

set owner

  $ fe change root -set-owners "jdoe1,jdoe3" -verbose
  ((Set_owners (jdoe1 jdoe3)) (Ok ()))

  $ fe show root -owners
  (jdoe1 jdoe3)

  $ fe show root -first-owner
  jdoe1

base

  $ fe change root -set-base $base1
  $ fe show root -base | sed -e "s/$base1.*/111/"
  111

  $ fe change root -set-base $base2
  $ fe show root -base | sed -e "s/$base2.*/222/"
  222

add whole feature reviewers

  $ fe change root -add-whole-feature-reviewers "jdoe4" -verbose
  ((Add_whole_feature_reviewers (jdoe4)) (Ok ()))

  $ fe show root -whole-feature-reviewers
  (jdoe4 unix-login-for-testing)

  $ fe change root -add-whole-feature-reviewers "jdoe1"

  $ fe change root -remove-owners "jdoe1" -verbose
  ((Remove_owners (jdoe1)) (Ok ()))
  $ fe change root -add-whole-feature-reviewers "jdoe1" -verbose
  ("there were problems"
   (((Add_whole_feature_reviewers (jdoe1))
     (Error ("already a whole-feature reviewer" (jdoe1))))))
  [1]
  $ fe show root -whole-feature-reviewers
  (jdoe1 jdoe4 unix-login-for-testing)

remove whole feature reviewers

  $ fe change root -remove-whole-feature-reviewers "jdoe3"
  ("there were problems"
   (((Remove_whole_feature_reviewers (jdoe3))
     (Error ("not currently a whole-feature reviewer" (jdoe3))))))
  [1]

  $ fe change root -remove-whole-feature-reviewers "jdoe1" -verbose
  ((Remove_whole_feature_reviewers (jdoe1)) (Ok ()))

  $ fe show root -whole-feature-reviewers
  (jdoe4 unix-login-for-testing)

set whole feature reviewers

  $ fe change root -set-whole-feature-reviewers "jdoe1,jdoe4" -verbose
  ((Set_whole_feature_reviewers (jdoe1 jdoe4)) (Ok ()))

  $ fe show root -whole-feature-reviewers
  (jdoe1 jdoe4)

is permanent

  $ fe show root -is-permanent
  false

  $ fe change root -set-is-permanent true -verbose
  ((Set_is_permanent true) (Ok ()))

  $ fe show root -is-permanent
  true

  $ fe change root -set-is-permanent false -verbose
  ((Set_is_permanent false) (Ok ()))

  $ fe show root -is-permanent
  false

description

  $ fe description show root
  root

  $ fe change root -set-description "description for root" -verbose
  ((Set_description "description for root") (Ok ()))

  $ fe description show root
  description for root

is review_enabled

  $ fe show root -review-is-enabled
  false
  $ fe enable-review root
  $ fe show root -review-is-enabled
  true
  $ fe disable-review root
  $ fe show root -review-is-enabled
  false

reviewing

  $ fe show root -reviewing
  (unix-login-for-testing)
  $ fe change root -set-reviewing ''
  $ fe show root -reviewing
  ()
  $ fe change root -add-reviewing a
  $ fe show root -reviewing
  (a)
  $ fe change root -add-reviewing b,c
  $ fe show root -reviewing
  (a b c)
  $ fe change root -remove-reviewing b
  $ fe show root -reviewing
  (a c)
  $ fe change root -remove-reviewing a,c
  $ fe show root -reviewing
  ()
  $ fe change root -set-reviewing a,b
  $ fe show root -reviewing
  (a b)
  $ fe change root -set-reviewing-whole-feature-only
  $ fe show root -reviewing
  "whole-feature reviewers"
  $ fe change root -set-reviewing all
  $ fe show root -reviewing
  all
  $ fe change root -set-reviewing-none
  $ fe show root -reviewing
  ()
  $ fe change root -set-reviewing-all
  $ fe show root -reviewing
  all
  $ fe change root -remove-reviewing a,b
  $ fe show root -reviewing
  ("all but" (a b))
  $ fe change root -set-reviewing-whole-feature-only
  $ fe change root -remove-reviewing jdoe1
  $ fe show root -reviewing
  (jdoe4)
  $ fe change root -set-reviewing-whole-feature-only
  $ fe change root -add-reviewing a,b,c
  $ fe show root -reviewing
  (a b c jdoe1 jdoe4)

-lock

  $ fe change -lock
  Use [fe lock] or [fe unlock].
  [1]

-set-should-send-release-mail

  $ fe show -send-email-upon
  (archive release)
  $ fe change -remove-send-email-upon release
  $ fe show | grep -F 'send email upon' | single_space
  | send email upon | archive |
  $ fe show -send-email-upon
  (archive)
  $ fe change -add-send-email-upon release
  $ fe show -send-email-upon
  (archive release)
  $ fe show | grep -F 'send email upon'
  [1]
