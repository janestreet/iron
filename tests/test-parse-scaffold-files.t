  $ start_test

Verify that Iron can understand the scaffold files that are used in prod.

  $ cat > scaffoo.sexp <<EOF
  > ((repo ssh://hg//hg/root/submissions)
  >  (id 2376769d8f3389d54e4d19bc45cadd1b8bea6840)
  >  (others (((dir scaffoo/) (repo self)))))
  > EOF

  $ fe internal scaffold print scaffoo.sexp
  ((center_relative_to_enclosing_repo scaffoo)
   (satellites
    (((repo_root .) (remote_repo_path ssh://hg//hg/root/submissions)
      (human_readable "root satellite")
      (revision 2376769d8f3389d54e4d19bc45cadd1b8bea6840)))))

  $ cat > scabar.sexp <<EOF
  > ((repo ssh://hg//hg/root/submissions) (id root-112.21+41)
  >   (others
  >   (((dir root-tools) (repo ssh://hg//hg/root-tools/submissions)
  >   (id root-tools-112.21.00+04))
  >   ((dir scabar/) (repo self))
  >   ((dir scabaz-protocols/) (repo ssh://hg//hg/scabaz-protocols/release)
  >   (id 1b42dfc80e78)))))
  > EOF

  $ fe internal scaffold print scabar.sexp
  ((center_relative_to_enclosing_repo scabar)
   (satellites
    (((repo_root .) (remote_repo_path ssh://hg//hg/root/submissions)
      (human_readable "root satellite") (revision root-112.21+41))
     ((repo_root root-tools)
      (remote_repo_path ssh://hg//hg/root-tools/submissions)
      (human_readable "root-tools satellite")
      (revision root-tools-112.21.00+04))
     ((repo_root scabaz-protocols)
      (remote_repo_path ssh://hg//hg/scabaz-protocols/release)
      (human_readable "scabaz-protocols satellite") (revision 1b42dfc80e78)))))
