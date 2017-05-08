open! Core
open! Import

type t =
  | V1
  | V2_sql_xml
[@@deriving compare, enumerate]

let equal t1 t2 = compare t1 t2 = 0

let to_int = function
  | V1 -> 1
  | V2_sql_xml -> 2
;;

let of_int = function
  | 1 -> V1
  | 2 -> V2_sql_xml
  | n -> raise_s [%sexp "Cr_comment_format: version is out of range", (n : int)]
;;

let sexp_of_t t = sexp_of_int (to_int t)

let t_of_sexp sexp = of_int (int_of_sexp sexp)

let latest = List.hd_exn (List.sort all ~cmp:(fun x y -> compare y x))

let is_after_format t ~format = compare t format >= 0
