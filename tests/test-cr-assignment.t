Start test. 

  $ start_test

Set up an alias.

  $ fe admin users update-valid-users-and-aliases -stdin <<EOF
  > ((username bfranklin) (alias (alternate-name)))
  > ((username jdoe4))
  > ((username unix-login-for-testing))
  > EOF
  $ fe admin users refresh-existing-users

Setup a repository with a few crs and cr soons.

  $ setup_repo_and_root file1.ml
  $ cat > file1.ml <<EOF
  > (* $CR jdoe4 for alternate-name: cr2 *)
  > (* $CR-soon jdoe4 for alternate-name: cr2 *)
  > EOF
  $ hg commit -m "add crs"
  $ feature_to_server root -fake-valid-obligations
  $ fe admin users refresh-existing-users

Check the resulting crs.

  $ fe internal dump user-info aliases
  ((alternate-name bfranklin))
  $ fe crs -for bfranklin
  file1.ml:1:1:
    CR jdoe4 for alternate-name: cr2 
  $ fe crs -soon -for bfranklin
  file1.ml:2:1:
    CR-soon jdoe4 for alternate-name: cr2 
  $ fe crs -for alternate-name
  $ fe crs -soon -for alternate-name

Bounce the server, and nothing should have changed.

  $ fe-server stop
  $ fe-server start

  $ fe internal dump user-info aliases
  ((alternate-name bfranklin))
  $ fe crs -for bfranklin
  file1.ml:1:1:
    CR jdoe4 for alternate-name: cr2 
  $ fe crs -soon -for bfranklin
  file1.ml:2:1:
    CR-soon jdoe4 for alternate-name: cr2 
  $ fe crs -for alternate-name
  $ fe crs -soon -for alternate-name

Remove [alternate-name] as an alias.

  $ fe admin users define-typos -typo alternate-name -means bfranklin \
  >     |& matches "alternate-name is already an alias for bfranklin"
  [1]
  $ fe admin users remove-aliases alternate-name non-existent-alias \
  >     |& matches "The following aliases were not removed.*(non-existent-alias)"
  [1]

Now [alternate-name] should have crs. [bfranklin] should have lost crs.

  $ fe crs -for bfranklin
  $ fe crs -soon -for bfranklin
  $ fe crs -for alternate-name
  file1.ml:1:1:
    CR jdoe4 for alternate-name: cr2 
  $ fe crs -soon -for alternate-name
  file1.ml:2:1:
    CR-soon jdoe4 for alternate-name: cr2 

Make alternate-name a typo now, and check error reporting:

  $ fe admin users define-typos -typo bfranklin -means bfranklin \
  >     |& matches "bfranklin is a valid user name"
  [1]
  $ fe admin users define-typos -typo alternate-name -means dcu \
  >     |& matches "\"never heard of users\" (dcu)"
  [1]
  $ fe admin users define-typos \
  >     -typo alternate-name -means bfranklin \
  >     -typo alternate-name -means jdoe4 \
  >     ;
  (error
   (define-typos
    ("typo can be resolved to different user names"
     ((typo alternate-name) (means (jdoe4 bfranklin))))))
  [1]
  $ fe admin users define-typos -typo alternate-name -means bfranklin

Now [alternate-name] has no crs. [bfranklin] should have gained them.

  $ fe crs -for bfranklin
  file1.ml:1:1:
    CR jdoe4 for alternate-name: cr2 
  $ fe crs -soon -for bfranklin
  file1.ml:2:1:
    CR-soon jdoe4 for alternate-name: cr2 
  $ fe crs -for alternate-name
  $ fe crs -soon -for alternate-name
