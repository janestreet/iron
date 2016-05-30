Test of various ways that jhg calls fe.  If any of the results in this
file change, please email app-hg-dev@ with information on the upcoming
changes.

  $ start_test
  $ setup_repo_and_root file

# jhg checks if a feature exists by confirming that
# [fe show $BOOKMARK -feature-path] returns $BOOKMARK
  $ fe show root -feature-path
  root

# This check should fail for abbreviations
  $ fe create root/child -d root/child
  $ fe show child -feature-path
  root/child

# It should also fail for nonexistent features
  $ fe show nonexistent-feature -feature-path &> /dev/null
  [1]

# jhg checks if a feature uses continuous release by checking if the
# output of [fe show -release-process $FEATURE] is "Continuous".

  $ fe show root -release-process
  Direct
  $ fe change root -set-release-process continuous
  $ fe show root -release-process
  Continuous
