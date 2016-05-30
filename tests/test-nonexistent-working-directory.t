  $ start_test
  $ mkdir z
  $ cd z
  $ ( cd .. && rmdir z ); fe help >/dev/null
  shell-init: error retrieving current directory: getcwd: cannot access parent directories: No such file or directory
  $ cd $HOME
