open! Core
open! Async
open! Import

type t =
  | Static of int
  | Dynamic of string
[@@deriving sexp]

let port = function
  | Static port -> return port
  | Dynamic file -> Reader.load_sexp_exn file [%of_sexp: int]
;;

let write_if_dynamic t ~port =
  match t with
  | Static _ -> Deferred.unit
  | Dynamic file -> Writer.save_sexp file ([%sexp_of: int] port)
;;
