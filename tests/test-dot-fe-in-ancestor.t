  $ start_test
  $ setup_repo_without_root
  $ mkdir .fe
  $ cat >.fe/obligations-global.sexp <<EOF
  > (Define_scrutiny normal ((level 50) (description "normal")))
  > (Users user1)
  > EOF
  $ cat >.fe/obligations-repo.sexp <<EOF
  > EOF
  $ hg add -q
  $ fe ob check
  ("no .fe.sexp in or above directory" .fe)
  [1]

Creating the .fe.sexp but not [hg add]ing it causes an "untracked" error message.

  $ touch .fe/.fe.sexp
  $ fe ob check
  ("untracked .fe.sexp file" .fe/.fe.sexp)
  [1]
  $ rm .fe/.fe.sexp

  $ touch .fe.sexp
  $ fe ob check
  ("untracked .fe.sexp file" .fe.sexp)
  [1]

[hg add] the file, and it is used, but there is still one missing in the .fe subdir.

  $ hg add .fe.sexp
  $ fe ob check
  ("directory missing .fe.sexp file" .fe)
  [1]

Add [Used_in_subdirectory] to fix that, but there are still problems.

  $ cat >.fe.sexp <<EOF
  > Used_in_subdirectory
  > EOF
  $ fe ob check
  ("invalid .fe.sexp files"
   ((.fe.sexp:0:0
     ("invalid file attributes" (((file .fe.sexp) (error "missing owner")))))
    (.fe.sexp:0:0
     (("used in subdirectory" .fe)
      ("invalid file attributes"
       (((file .fe.sexp) (error "missing owner"))
        ((file obligations-global.sexp) (error "missing owner"))
        ((file obligations-repo.sexp) (error "missing owner"))))))))
  [1]

Removing the file causes a "deleted" error message.

  $ rm .fe.sexp
  $ fe ob check
  ("deleted but tracked .fe.sexp file" .fe.sexp)
  [1]

Fix the .fe.sexp, and obligations are valid.

  $ cat >.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Apply_to All_files)
  > Used_in_subdirectory
  > EOF
  $ fe ob check
  $ hg com -m good

It's OK to [fe ob show] a file that has a [Used_in_subdirectory], even
though that doesn't check whether it's used.

  $ fe ob show .fe.sexp >/dev/null

We get a reasonable error if the .fe.sexp works for its own directory,
but breaks a descendant directory.

  $ cat >.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Apply_to (Files .fe.sexp))
  > Used_in_subdirectory
  > EOF
  $ fe ob check
  ("invalid .fe.sexp file"
   (.fe.sexp:0:0
    (("used in subdirectory" .fe)
     ("invalid file attributes"
      (((file obligations-global.sexp) (error "missing owner"))
       ((file obligations-repo.sexp) (error "missing owner")))))))
  [1]
  $ hg revert -q .

"used in subdirectory" in error message is relative to the [.fe.sexp]'s directory.

  $ mkdir -p foo/bar
  $ cat >foo/.fe.sexp <<EOF
  > (Owner user1)
  > (Scrutiny normal)
  > (Apply_to (Files .fe.sexp))
  > Used_in_subdirectory
  > EOF
  $ touch foo/bar/baz
  $ hg add -q
  $ fe ob check
  ("invalid .fe.sexp file"
   (foo/.fe.sexp:0:0
    (("used in subdirectory" bar)
     ("invalid file attributes" (((file baz) (error "missing owner")))))))
  [1]
  $ hg revert -q .

An error if a .fe.sexp file has a [Used_in_subdirectory] declaration,
but isn't actually used in a subdirectory.

  $ cp .fe.sexp .fe/.fe.sexp
  $ hg add .fe/.fe.sexp
  $ fe ob check
  ("invalid .fe.sexp files"
   ((.fe.sexp:4:0 "unnecessary Used_in_subdirectory declaration")
    (.fe/.fe.sexp:4:0 "unnecessary Used_in_subdirectory declaration")))
  [1]
