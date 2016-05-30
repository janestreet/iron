  $ start_test

  $ cat > foo.ml <<EOF
  > # $CR-someday foo: would be nicer if it
  > # rhymed.
  > Once upon a time
  > there was a piece of code
  > with CRs all over it # $XCR-soon foo: it's spelt CR
  > and it never (* $CR foo for whoever: asd *) got released.
  > The end.
  > EOF

  $ fe tools strip-crs foo.ml
  Once upon a time
  there was a piece of code
  with CRs all over it
  and it never  got released.
  The end.

  $ fe tools strip-crs foo.ml -replace-with CENSORED
  # CENSORED
  Once upon a time
  there was a piece of code
  with CRs all over it # CENSORED
  and it never (* CENSORED*) got released.
  The end.

  $ fe tools strip-crs foo.ml -in-place
  $ cat foo.ml
  Once upon a time
  there was a piece of code
  with CRs all over it
  and it never  got released.
  The end.
