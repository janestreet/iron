  $ start_test

Setup a repo with a few crs.

  $ hg init repo
  $ cd repo
  $ cat >file1.ml <<EOF
  > (* $CR user1 for user2: OCaml *)
  > (** $CR user1 for user2: OCaml docstring *)
  > (* $CR user1: OCaml with (* nested comment *) inside *)
  > /* $CR user1 for user2: C-style */
  > /* $CR user1 for user2: C-style does /* not */ nest */
  > /** $CR user1 for user2: C-style docstring */
  > // $CR user1 for user2: C++
  > <!-- $CR user1 for user2: XML -->
  > <!-- $CR user1 for user2: XML
  > // multiline -->
  > ; $CR user2 for user1: Lisp
  > ; multiline
  > // $XCR user1 for user2: C++
  > // multiline
  > # $CR user1: shell
  > 
  > # $CR user1: shell
  > # multiline
  > 
  > // $CR user1: C++ that has
  > -/ NOT PART OF THE CR
  > -- $CR user1: SQL
  > -- multiline
  > */ NOT PART OF THE CR
  > EOF
  $ mkdir .fe; touch .fe/obligations-repo.sexp .fe/obligations-global.sexp
  $ hg add file1.ml .fe/obligations-repo.sexp .fe/obligations-global.sexp
  $ hg commit -m "add crs"
  $ fe create -tip . test -remote "$(pwd)" -desc 'root for test'

Grep using all defaults.

  $ v1=$(fe crs -grep -for all)
  $ echo "$v1"
  file1.ml:1:1:
    CR user1 for user2: OCaml 
  
  file1.ml:2:1:
    CR user1 for user2: OCaml docstring 
  
  file1.ml:3:1:
    CR user1: OCaml with (* nested comment *) inside 
  
  file1.ml:4:1:
    CR user1 for user2: C-style 
  
  file1.ml:5:1:
    CR user1 for user2: C-style does /* not 
  
  file1.ml:6:1:
    CR user1 for user2: C-style docstring 
  
  file1.ml:7:1:
    CR user1 for user2: C++
  
  file1.ml:11:1:
  CR user2 for user1: Lisp
  ; multiline
  
  file1.ml:13:1:
  XCR user1 for user2: C++
  // multiline
  
  file1.ml:15:1:
    CR user1: shell
  
  file1.ml:17:1:
  CR user1: shell
  # multiline
  
  file1.ml:20:1:
    CR user1: C++ that has

Grep again, using (Cr_comment_format 2).

  $ cat >.fe/obligations-global.sexp <<EOF
  > (Cr_comment_format 2)
  > EOF

  $ v2=$(fe crs -grep -for all)
  $ diff -U 0 <(echo "$v1") <(echo "$v2") | tail -n +3
  @@ -21,0 +22,7 @@
  +file1.ml:8:1:
  +  CR user1 for user2: XML 
  +
  +file1.ml:9:1:
  +CR user1 for user2: XML
  +// multiline 
  +
  @@ -38,0 +46,4 @@
  +
  +file1.ml:22:1:
  +CR user1: SQL
  +-- multiline
  [1]
