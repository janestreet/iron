(* open Core *)

type 'a t = {
  from : 'a;
  to_  : 'a;
} [@@deriving sexp_of, fields]
;;

let _map t ~f = {
  from = f t.from;
  to_  = f t.to_;
}
;;
