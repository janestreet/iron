  $ start_test

Adding one property

  $ setup_repo_and_root a
  $ fe show | grep prop
  [1]
  $ fe change -set-property prop1=value1
  $ fe show | grep prop | single_space
  | prop1 | value1 |
  $ fe show -property prop1
  value1

  $ fe show -property no-such-prop |& matches "undefined property"
  [1]

Checking persistence

  $ fe-server stop
  $ fe-server start
  $ fe show | grep prop | single_space
  | prop1 | value1 |

Several properties in the same feature

  $ fe change -set-property prop2=value2
  $ fe change -set-property prop3=value3
  $ fe show | grep prop | single_space
  | prop1 | value1 |
  | prop2 | value2 |
  | prop3 | value3 |

Removing properties

  $ fe change -remove-property prop2,prop1
  $ fe show | grep prop | single_space
  | prop3 | value3 |
  $ fe change -remove-property prop3,no-such-prop |& matches "unknown properties"
  [1]
  $ fe show | grep prop | single_space
  | prop3 | value3 |

Overwriting properties

  $ fe change -set-property prop3=value3-again
  $ fe show | grep prop | single_space
  | prop3 | value3-again |

Pretty-printing properties

  $ fe change -set-property 'prop4=(rev((human_readable(jane-114.13+51))(node_hash 85f4645b2f8bcaf3afb94e6a393fa41104da6071)))'
  $ COLUMNS=120 fe show | grep -A 3 prop3 | single_space
  | prop3 | value3-again |
  | prop4 | (rev |
  | | ((human_readable (jane-114.13+51)) |
  | | (node_hash 85f4645b2f8bcaf3afb94e6a393fa41104da6071))) |
