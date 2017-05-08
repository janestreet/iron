open Core

let ok_or_bug x =
  match x with
  | Ok okval -> okval
  | Error e  -> raise_s [%sexp "Program bug!", (e : Error.t)]
;;
