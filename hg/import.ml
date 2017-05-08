open! Core

include Iron_common.Std
include Iron_obligations.Std
include Int.Replace_polymorphic_compare

let line_count string =
  let has_newline_at i = Char.equal (string.[ i ]) '\n' in
  let n = String.length string in
  if n = 0
  then 0
  else (
    let r = ref 0 in
    for i = 0 to n - 1 do
      if has_newline_at i then incr r;
    done;
    if not (has_newline_at (n - 1)) then incr r;
    !r)
;;

let ignore_worker_invariant_on_server _ = ()

let%test _ = line_count "" = 0
let%test _ = line_count "a" = 1
let%test _ = line_count "a\n" = 1
let%test _ = line_count "a\nb" = 2
let%test _ = line_count "a\nb\n" = 2

let (>>>) = `Deprecated_in_iron__Use_let_syntax_instead
let exit  = `Deprecated_in_iron__Use_shutdown_dot_exit_instead
let failwiths = `Deprecated_in_iron__Use_raise_s

let () = print_elapsed [%here]
