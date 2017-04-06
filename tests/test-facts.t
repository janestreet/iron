Start test
  $ start_test

Can't assert a fact with no existing spec
  $ fe fact add -spec-id "test" -scope "((rev 1))" -comment "test comment"
  (error (fact-action ("no spec defined for id" test)))
  [1]

Add spec
  $ fe fact spec add -spec-id "test" -spec-file /dev/stdin <<EOF
  > ((scope_keys (rev))
  >  (authorization_rules (((asserters (unix-login-for-testing)))))
  >  (description test))
  > EOF

Assert fact
  $ fe fact add -spec-id "test" -scope "((rev 1))" -comment "test comment"
  $ fe fact show -spec-id "test" -scope "((rev 1))" 
  asserter: unix-login-for-testing
  assertion_time: * (glob)
  comment: test comment

Remove spec
  $ fe fact spec remove -spec-id "test"

Can still check evidence of fact whose spec has been removed
  $ fe fact show -spec-id "test" -scope "((rev 1))" 
  asserter: unix-login-for-testing
  assertion_time: * (glob)
  comment: test comment

But can't assert new fact whose spec has been removed
  $ fe fact add -spec-id "test" -scope "((rev 1))" -comment "test comment"
  (error (fact-action ("no spec defined for id" test)))
  [1]

Retract fact
  $ fe fact remove -spec-id "test" -scope "((rev 1))"
  $ fe fact show -spec-id "test" -scope "((rev 1))" 
  (error
   (fact-evidence ("no evidence for scope" ((scope ((rev 1))) (spec_id test)))))
  [1]

Add spec back, different authorization rules
  $ fe fact spec add -spec-id "test" -spec-file /dev/stdin <<EOF
  > ((scope_keys (rev))
  >  (authorization_rules (((asserters (non-existent-user)))))
  >  (description test))
  > EOF

User not authorized
  $ fe fact add -spec-id "test" -scope "((rev 1))" -comment "test comment"
  (error
   (fact-action
    ("user not authorized for assertion" (test unix-login-for-testing))))
  [1]

Add spec
  $ fe fact spec add -spec-id "test" -spec-file /dev/stdin <<EOF
  > ((scope_keys (rev))
  >  (authorization_rules (((asserters (unix-login-for-testing)))))
  >  (description test))
  > EOF

Should be able to assert fact again
  $ fe fact add -spec-id "test" -scope "((rev 1))" -comment "test comment"

Check evidence for fact
  $ fe fact show -spec-id "test" -scope "((rev 1))" 
  asserter: unix-login-for-testing
  assertion_time: * (glob)
  comment: test comment

Assert another fact
  $ fe fact add -spec-id "test" -scope "((rev 2))" -comment "another test comment"

Check evidence for fact
  $ fe fact show -spec-id "test" -scope "((rev 2))" 
  asserter: unix-login-for-testing
  assertion_time: * (glob)
  comment: another test comment

Check evidence for fact, in machine format
  $ fe fact show -spec-id "test" -scope "((rev 2))" -machine
  ((asserter unix-login-for-testing)(assertion_time(*))(comment"another test comment")) (glob)

List all evidence for spec
  $ fe fact list -spec-id "test" 
  (((rev 1))
   ((asserter unix-login-for-testing)
    (assertion_time (*)) (comment "test comment"))) (glob)
  (((rev 2))
   ((asserter unix-login-for-testing)
    (assertion_time (*)) (glob)
    (comment "another test comment")))

Show specs
  $ fe fact spec list
  |-------------------------------------------------------------|
  | spec id | description | scope keys | asserters              |
  |---------+-------------+------------+------------------------|
  | test    | test        | rev        | unix-login-for-testing |
  |-------------------------------------------------------------|
  $ fe fact spec list -machine
  (test((scope_keys(rev))(authorization_rules(((asserters(unix-login-for-testing)))))(description test)))

Test persistence.

  $ fe-server stop
  $ fe-server start
  $ fe fact spec list
  |-------------------------------------------------------------|
  | spec id | description | scope keys | asserters              |
  |---------+-------------+------------+------------------------|
  | test    | test        | rev        | unix-login-for-testing |
  |-------------------------------------------------------------|
  $ fe fact show -spec-id "test" -scope "((rev 2))" -machine
  ((asserter unix-login-for-testing)(assertion_time(*))(comment"another test comment")) (glob)
