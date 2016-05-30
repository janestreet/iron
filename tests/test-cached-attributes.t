Start test.

  $ start_test

Create repo.

  $ mkdir repo
  $ cd repo
  $ hg init
  $ touch f1.ml
  $ hg add f1.ml
  $ hg commit -m "0"

Create some features.

  $ fe create root -description 'root' -remote-repo-path $(pwd)
  $ echo change >f1.ml; hg com -m change
  $ feature_to_server root -fake-valid
  $ fe create root/a -description 'a'

Establish original next steps.

  $ feature_to_server root/a -fake-valid
  $ fe show a -next-step
  (Add_code)
  $ fe show root -next-step
  (Enable_review)

Check consistency.

  $ fe internal cached-attributes check -all

Force set.  The cached gets fixed and we record the inconsistency.

  $ fe internal cached-attributes force-set root -next-steps '(Add_code)' \
  >   |& matches 'root.*fixed incorrect cache.*was_cached_as.*Add_code.*set_cache_to.*Enable_review'
  [1]

  $ fe-server stop
  $ export IRON_FUNCTIONAL_TESTING_CACHED_ATTRIBUTES_ERRORS=
  $ fe-server start

  $ fe internal cached-attributes force-set root -next-steps '(Add_code)'
  $ fe show root -next-step
  (Enable_review)
  $ fe internal cached-attributes check root
  $ fe internal cached-attributes errors get \
  >     |& matches 'post-RPC check.*root.*fixed incorrect cache.*was_cached_as.*Add_code.*set_cache_to.*Enable_review'
  [1]
  $ fe internal cached-attributes errors clear
  $ fe internal cached-attributes errors get

Force set incorrect value again, this time skip the post-RPC check.
We still have a global check that we can run to check everything periodically.

  $ fe internal cached-attributes force-set root -skip-post-rpc-check -next-steps '(Add_code)'
  $ fe show root -next-step
  (Add_code)
  $ fe internal cached-attributes check root \
  >     |& matches 'root.*fixed incorrect cache.*was_cached_as.*Add_code.*set_cache_to.*Enable_review'
  [1]

Verify that the check has fixed the incorrect attributes.

  $ fe show root -next-step
  (Enable_review)

Test invalidation rpc.

  $ fe internal cached-attributes force-set root -skip-post-rpc-check -next-steps '(Add_code)'
  $ fe show root -next-step
  (Add_code)
  $ fe internal cached-attributes invalidate root
  $ fe show root -next-step
  (Enable_review)

