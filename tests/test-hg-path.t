This is not so much a test of [fe internal hg-path], but a test to ensure
that these two hg's match:
1. the one that fe uses
2. the one that these test scripts use when they simply invoke [hg] on the command
|  line, and trust to $PATH lookup as provided by run-test and run-test.py.

See the --with-hg flag passed by run-test to run-test.py for the details on how #2
is set.

  $ setup_test
  $ diff -u --label "hg in path" <(which hg) --label "hg used by fe" <(fe internal hg-path)
