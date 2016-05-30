  $ start_test

  $ diff -u \
  >    <(fe internal rpc-to-server supported-by-client) \
  >    <(fe internal rpc-to-server supported-by-server)

  $ diff -u \
  >    <(fe internal command-rpc supported-by-iron-lib) \
  >    <(fe internal command-rpc supported-by-command)

  $ diff -u \
  >    <(fe internal command-rpc supported-by-iron-lib -names-only) \
  >    <(fe internal command-rpc referenced-by-fe-file)
