  $ setup_test
  $ unset TERM COLUMNS
  $ fe internal terminal-width
  90
  $ TERM=xterm fe internal terminal-width
  80
  $ TERM=xterm COLUMNS=42 fe internal terminal-width
  42
  $ TERM=foo fe internal terminal-width
  90
