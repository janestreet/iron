# Set up a bunch of revsets, named by various shell variables, for the various
# rebase tests. Because we set shell vars, this bash code must be sourced.
start_test

# function logit() { echo -n "$1: " ; date ; }

# Note: the "(<command> || true) |& fgrep <error-message>" idiom used repeatedly
# in this script is because we have pipefail set. If we want to observe a command
# failing by grepping for an error message, we need the "|| true" to keep the
# command's exit status from blowing up the whole pipeline.
#
# Define a utility function we'll use to do our various rebases:
# - $1 $2 $3: old-base, new-base, and feature revset
# - $4:    empty string or -fake-valid-obligations, for the root feature
# - $5:    empty string or -fake-valid-obligations, for the root/test-feature feature
# Set the server's understanding to the above facts, then do a rebase.
# If the rebase wins, the post-rebase revset is assigned to $diamond.

# Side-effects the working directory, due to the feature_to_server invocations.
#
# The root feature hackery is done in the "remote" repo.
# The root/test-feature hackery & the rebase are done in the local repo,
#   which should induce some pulling.

function rb_diamond {
  (cd "$remote_repo_dir"
   #echo fe-sb remote root "$1:"
   fe change root -set-base "$1"
   #echo set-book remote root "$2:"
   hg book -q -f -r "$2" root
   #echo f2s root:
   feature_to_server root $4 &>/dev/null )

  (cd "$local_repo_dir"
   #echo fe-sb local feature "$1:"
   fe change root/test-feature -set-base "$1"
   #echo set-book local feature "$3:"
   hg book -q -f -r "$3" root/test-feature
   #echo f2s feature:
   feature_to_server root/test-feature $5 &>/dev/null
   #echo rebase:
   rebase_cmd=$(echo "fe rebase root/test-feature -interactive true" ${REBASE_OPT-''})
   IRON_OPTIONS='((workspaces false))' \
     $rebase_cmd && diamond=$(hg tip --template={node}) )
  }

# Here's a little table of the revsets we are going to create.
# - All contain just one file: f1.txt.
# - Revsets named $root_* are in the root feature;
# - ones named $feature_* are in its root/test-feature child feature.
# - $r0 is in both -- it's the root revset with the original copy of f1.txt.
# $r0:               revision 0
# $root_tip          revision that will merge/rebase cleanly with $feature_tip.
# $root_alta:        alternate version that will conflict with $root_altb
# $root_altb:        alternate version that will conflict with $root_alta
# $root_conflict:    contains conflict markers from merging above two.
# $root_noncompat:   will conflict when merged with $feature_tip on a rebase
# $feature_tip:      will merge cleanly with $r0
# $feature_alt:      alternate version to generate conflict with $feature_tip
# $feature_conflict: contains conflict markers from merging above two
# Note:
# - We make actual conflicts with unresolvable merges instead of
#   just hard-coding the conflicted file into this script so this script will
#   track the kinds of conflict markers made by hg.
# - The values of these variables are {node} hashes, not {rev} counts, so they
#   can be used in both the local and remote repo.

# Create hg remote & local repos:

remote_repo_dir="$PWD/remote"
local_repo_dir="$PWD/local"
mkdir "$remote_repo_dir" "$local_repo_dir"
cd "$remote_repo_dir"
hg init

# All these root revsets are made only in the remote repo.
cat > f1.txt <<EOF
a
b
c
EOF
hg add f1.txt
hg commit -q -m init
r0=$(hg tip --template={node})
remote="$remote_repo_dir"

hg clone `pwd` "$local_repo_dir"
cat > $local_repo_dir/.hg/hgrc <<EOF
[paths]
default = ssh://hg//hg/root/fake-submissions
EOF

# Make the root feature:

fe create root -description 'root feature' -remote-repo-path $(pwd)

# Make an alternate revset with a conflict:

cat > f1.txt <<EOF
a
b
c
base-alternate-a
EOF
hg commit -q -m base-alta
root_alta=$(hg tip --template={node})

hg up -q -r "$r0"
cat > f1.txt <<EOF
a
b
c
base-alternate-b
EOF
hg commit -q -m base-altb
root_altb=$(hg tip --template={node})

# Make a conflicting base revset -- that is, a revset with the conflict markers
# in f1.txt committed:

hg merge -q -r "$root_alta"
hg resolve -m f1.txt
rm f1.txt.orig
hg commit -q -m base-conflict
root_conflict=$(hg tip --template={node})

# Do some hacking on the base:

hg up -q -r "$r0"
cat > f1.txt <<EOF
a
b
base-insert
c
EOF
hg commit -q -m "Hacked on the base."
root_tip=$(hg tip --template={node})

# Now jump over to the local repo.
cd "$local_repo_dir"

# Make the root/test-feature feature:
# Base & tip will be root's tip, that is, rev 0.

fe create root/test-feature -description 'test feature'

# Do some feature hacking -- insert a line into f1.txt.

cat > f1.txt <<EOF
a
feature-insert
b
c
EOF
hg commit -q -m "Hacked on the feature."
feature_tip=$(hg tip --template={node})

# Make a feature revset with conflict markers:

hg up -q -r "$r0"
cat > f1.txt <<EOF
a
feature-conflict
b
c
EOF
hg commit -q -m "Separate hacking on the feature."
feature_alt=$(hg tip --template={node})
hg up -q root/test-feature
hg merge -q -r "$feature_alt"
hg resolve -m f1.txt
rm f1.txt.orig
hg commit -q -m feature-conflict
feature_conflict=$(hg tip --template={node})

# Go to local repo & reset to a standard revset

cd "$local_repo_dir"
hg book -f -r "$feature_tip" root/test-feature
hg up -r root/test-feature

# $ logit "starting tests"

