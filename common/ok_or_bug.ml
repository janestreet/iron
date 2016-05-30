open Core.Std

let ok_or_bug x =
  match x with
  | Ok okval -> okval
  | Error e  -> failwiths "Program bug!" e [%sexp_of: Error.t]
;;
