  $ start_test

Setup a empty feature containing files with many lines

  $ hg init repo
  $ cd repo
  $ for i in {1..100}; do echo a >> a; done
  $ sed s/a/b/ < a > b
  $ hg add a b; hg commit -m 'a and b'
  $ fe create root -remote "$PWD" -d d
  $ fe create root/feature -d d
  $ hg -q update -r root/feature
  $ fe enable

Do renames and copies in the feature, and check that we don't review addition and
deletions, as evidenced by the small number of lines to review.
For now copies aren't handled, otherwise the line count would be lower and the fe
review would mention (path_in_repo b)

  $ hg mv a a-renamed; hg cp b b-copied
  $ hg commit -m 'mv a and copy b'
  $ feature_to_server root/feature -fake-valid-obligations
  $ fe internal session show-num-lines root/feature
  102
  $ fe review | grep path_in_repo | sed 's/^\s*//'
  ((path_in_repo a)
  ((path_in_repo a)
  ((path_in_repo a)
  ((path_in_repo a-renamed)
  ((path_in_repo b-copied)
  ((path_in_repo b-copied)
  ((path_in_repo b-copied)
  ((path_in_repo b-copied)

And now, when the feature is rebased, nothing special should happen,
in particular no conflicts between the redundant rename information
from the various edges of the diamond.

  $ fe tools mark-fully-reviewed root/feature -for all -reason reason
  $ hg -q update -r root
  $ echo c > c; hg add c; hg -q commit -m 2
  $ feature_to_server root -fake-valid-obligations
  $ fe tools mark-fully-reviewed root -for all -reason reason
  $ fe rebase root/feature > /dev/null
  $ echo changed >> a-renamed; hg commit -m 4
  $ feature_to_server root/feature -fake-valid-obligations
  $ fe session show
  Reviewing root/feature to *. (glob)
  1 files to review: 1 lines total
     [ ] 1 a-renamed
  $ fe session diff | grep -i renaming
  [1]
