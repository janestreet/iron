Start test.

  $ function is_int() { return $(test "$@" -eq "$@" > /dev/null 2>&1); }
  $ is_int 42
  $ is_int "not-an-int"
  [2]

  $ start_test

  $ FIRST=$(fe admin server stat -kind gc-stat | sexp select compactions)
  $ is_int ${FIRST}

Gc-compact.

  $ fe admin server gc-compact

  $ SECOND=$(fe admin server stat -kind gc-stat | sexp select compactions)
  $ is_int ${SECOND}

  $ test ${SECOND} -gt ${FIRST}
